package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.api.HttpError.{BadRequest, NotFound}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.{AuthenticationProvider, TokenAuthentication, ValidToken}
import com.joliciel.jochre.search.api.{HttpError, PngCodecFormat}
import com.joliciel.jochre.search.core.service.{
  Highlight,
  HighlightedDocument,
  SearchHelper,
  SearchProtocol,
  SearchResponse
}
import com.joliciel.jochre.search.core.{AggregationBins, DocReference, IndexField}
import com.typesafe.config.ConfigFactory
import io.circe.generic.auto._
import sttp.capabilities.zio.ZioStreams
import sttp.model.{Header, MediaType, StatusCode}
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.ztapir._
import sttp.tapir.{AnyEndpoint, CodecFormat}

import java.nio.charset.StandardCharsets
import scala.concurrent.ExecutionContext
import zio.stream.ZStream

case class SearchApp(override val authenticationProvider: AuthenticationProvider, executionContext: ExecutionContext)
    extends TokenAuthentication
    with SearchLogic
    with SearchProtocol
    with SearchSchemaSupport {
  given ExecutionContext = executionContext

  private val config = ConfigFactory.load().getConfig("jochre.search")
  private val allowSearchWithoutAuth = config.getBoolean("allow-search-without-auth")

  private val queryInput = query[Option[String]]("query")
    .description("Query string for searching in the text")
    .example(Some("קעלבעל"))
  private val titleInput =
    query[Option[String]]("title").description("Query string for searching in the title").example(Some("מאָטעל"))
  private val authorsInput =
    query[List[String]]("authors").description("Authors to include or exclude").example(List("שלום עליכם"))
  private val authorIncludeInput = query[Option[Boolean]]("author-include")
    .description("Whether the authors should be included or excluded. Default is true.")
    .example(Some(true))
  private val strictInput = query[Option[Boolean]]("strict")
    .description(
      "Whether query strings should be expanded to related synonyms (false) or not (true). Default is false."
    )
    .example(Some(false))
  private val fromYearInput =
    query[Option[Int]]("from-year").description("The earliest year of publication").example(Some(1900))
  private val toYearInput =
    query[Option[Int]]("to-year").description("The latest year of publication").example(Some(1920))
  private val docRefsInput =
    query[List[String]]("doc-refs").description("Which document references to include").example(List("nybc200089"))
  private val firstInput =
    query[Int]("first").description("The first result to return on the page of results").example(20)
  private val maxInput =
    query[Int]("max").description("The max number of results to return on the page of results").example(10)
  private val maxSnippetsInput =
    query[Option[Int]]("max-snippets").description("The maximum number of snippets per result").example(Some(20))
  private val rowPaddingInput = query[Option[Int]]("row-padding")
    .description("How many rows to add in the snippet before the first highlight and after the last highlight")
    .example(Some(2))
  private val sortInput = query[Option[String]]("sort")
    .description(f"The sort order (optional), among: ${SortKind.values.map(_.entryName).mkString(", ")}")
    .example(None)
  private val ocrSoftwareInput =
    query[Option[String]]("ocr-software").description("OCR Software version").example(Some("Jochre 3.0.0"))

  private val physicalNewlinesInput =
    query[Option[Boolean]]("physical-newlines")
      .description("Whether physical newlines should be maintained or removed from the snippets. Default is true.")
      .example(Some(false))

  private val docReferenceInput = query[DocReference]("doc-ref")
    .description("The document containing the word or image")
    .example(DocReference("nybc200089"))
  private val startOffsetInput = query[Int]("start-offset")
    .description("The start character offset of the first word with respect to the entire document (inclusive)")
    .example(10200)
  private val endOffsetInput = query[Int]("end-offset")
    .description("The end character offset of the last word with respect to the entire document (exclusive)")
    .example(10450)
  private val highlightInput = query[List[Highlight]]("highlight")
    .description(
      "A list of words to highlight using the start index and end index of each word or contiguous list of words" +
        ", e.g. \"[10210,10215],[10312,10320]\""
    )

  private val noAuthSuffix = "-no-auth"
  private val noAuthDescription = " No authorization required."
  private val badRequestUnparsableQuery =
    oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparsable query"))

  private val getSearchDescription = "Search the OCR index."

  private val getSearchEndpointNoAuth =
    insecureEndpoint
      .errorOut(oneOf[HttpError](badRequestUnparsableQuery))
      .get
      .in("search" + noAuthSuffix)
      .in(queryInput)
      .in(titleInput)
      .in(authorsInput)
      .in(authorIncludeInput)
      .in(strictInput)
      .in(fromYearInput)
      .in(toYearInput)
      .in(docRefsInput)
      .in(firstInput)
      .in(maxInput)
      .in(maxSnippetsInput)
      .in(rowPaddingInput)
      .in(sortInput)
      .in(physicalNewlinesInput)
      .in(clientIp)
      .out(jsonBody[SearchResponse].example(SearchHelper.searchResponseExample))
      .description(getSearchDescription + noAuthDescription)

  private val getSearchHttpNoAuth: ZServerEndpoint[Requirements, Any] =
    getSearchEndpointNoAuth.zServerLogic[Requirements](input => getSearchLogicNoAuth.tupled(input))

  private val getSearchEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        Option[String],
        Option[String],
        List[String],
        Option[Boolean],
        Option[Boolean],
        Option[Int],
        Option[Int],
        List[String],
        Int,
        Int,
        Option[Int],
        Option[Int],
        Option[String],
        Option[Boolean],
        Option[String]
    ),
    HttpError,
    SearchResponse,
    Any
  ] =
    secureEndpoint()
      .errorOutVariantPrepend[HttpError](badRequestUnparsableQuery)
      .get
      .in("search")
      .in(queryInput)
      .in(titleInput)
      .in(authorsInput)
      .in(authorIncludeInput)
      .in(strictInput)
      .in(fromYearInput)
      .in(toYearInput)
      .in(docRefsInput)
      .in(firstInput)
      .in(maxInput)
      .in(maxSnippetsInput)
      .in(rowPaddingInput)
      .in(sortInput)
      .in(physicalNewlinesInput)
      .in(clientIp)
      .out(jsonBody[SearchResponse].example(SearchHelper.searchResponseExample))
      .description(getSearchDescription)

  private val getSearchHttp: ZServerEndpoint[Requirements, Any] =
    getSearchEndpoint.serverLogic[Requirements](token => input => getSearchLogic.tupled(Tuple1(token) ++ input))

  private val badRequestOffsetsOnDifferentPages = oneOfVariant[BadRequest](
    StatusCode.BadRequest,
    jsonBody[BadRequest].description(
      "Offsets refer to words on different pages, or are higher than document length."
    )
  )

  private val notFoundDocRef = oneOfVariant[NotFound](
    StatusCode.NotFound,
    jsonBody[NotFound].description(
      "Requested document reference not found in index."
    )
  )

  private val getImageSnippetDescription = "Return an image snippet in PNG format."

  private val getImageSnippetEndpointNoAuth =
    insecureEndpoint
      .errorOut(oneOf[HttpError](badRequestOffsetsOnDifferentPages, notFoundDocRef))
      .get
      .in("image-snippet" + noAuthSuffix)
      .in(docReferenceInput)
      .in(startOffsetInput)
      .in(endOffsetInput)
      .in(highlightInput)
      .in(clientIp)
      .out(header(Header.contentType(MediaType.ImagePng)))
      .out(streamBinaryBody(ZioStreams)(PngCodecFormat))
      .description(getImageSnippetDescription + noAuthDescription)

  private val getImageSnippetHttpNoAuth: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getImageSnippetEndpointNoAuth.zServerLogic[Requirements](input => getImageSnippetLogicNoAuth.tupled(input))

  private val getImageSnippetEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        DocReference,
        Int,
        Int,
        List[Highlight],
        Option[String]
    ),
    HttpError,
    ZStream[Any, Throwable, Byte],
    Any & ZioStreams
  ] =
    secureEndpoint()
      .errorOutVariantsPrepend[HttpError](badRequestOffsetsOnDifferentPages, notFoundDocRef)
      .get
      .in("image-snippet")
      .in(docReferenceInput)
      .in(startOffsetInput)
      .in(endOffsetInput)
      .in(highlightInput)
      .in(clientIp)
      .out(header(Header.contentType(MediaType.ImagePng)))
      .out(streamBinaryBody(ZioStreams)(PngCodecFormat))
      .description(getImageSnippetDescription)

  private val getImageSnippetHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getImageSnippetEndpoint.serverLogic[Requirements](token =>
      input => getImageSnippetLogic.tupled(Tuple1(token) ++ input)
    )

  private val getImageSnippetWithHighlightsDescription =
    "Return an image snippet in PNG format converted to Base64 and a list of relative rectangles to highlight."

  private val getImageSnippetWithHighlightsEndpointNoAuth =
    insecureEndpoint
      .errorOut(oneOf[HttpError](badRequestOffsetsOnDifferentPages, notFoundDocRef))
      .get
      .in("image-snippet-with-highlights" + noAuthSuffix)
      .in(docReferenceInput)
      .in(startOffsetInput)
      .in(endOffsetInput)
      .in(highlightInput)
      .in(clientIp)
      .out(jsonBody[ImageSnippetResponse])
      .description(
        getImageSnippetWithHighlightsDescription + noAuthDescription
      )

  private val getImageSnippetWithHighlightsHttpNoAuth: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getImageSnippetWithHighlightsEndpointNoAuth.zServerLogic[Requirements](input =>
      getImageSnippetWithHighlightsLogicNoAuth.tupled(input)
    )

  private val getImageSnippetWithHighlightsEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        DocReference,
        Int,
        Int,
        List[Highlight],
        Option[String]
    ),
    HttpError,
    ImageSnippetResponse,
    Any & ZioStreams
  ] =
    secureEndpoint()
      .errorOutVariantsPrepend[HttpError](badRequestOffsetsOnDifferentPages, notFoundDocRef)
      .get
      .in("image-snippet-with-highlights")
      .in(docReferenceInput)
      .in(startOffsetInput)
      .in(endOffsetInput)
      .in(highlightInput)
      .in(clientIp)
      .out(jsonBody[ImageSnippetResponse])
      .description(
        getImageSnippetWithHighlightsDescription
      )

  private val getImageSnippetWithHighlightsHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getImageSnippetWithHighlightsEndpoint.serverLogic[Requirements](token =>
      input => getImageSnippetWithHighlightsLogic.tupled(Tuple1(token) ++ input)
    )

  private val aggregateFieldInput = query[String]("field")
    .description(f"The field to choose among ${IndexField.aggregatableFields.map(_.entryName).mkString(", ")}")
    .example(IndexField.Author.entryName)

  private val maxBinsInput = query[Option[Int]]("maxBins")
    .description("Maximum bins to return. If not provided, all bins will be returned.")
    .example(Some(20))

  private val sortByLabelInput = query[Option[Boolean]]("sortByLabel")
    .description(
      "If true, aggregated bins will be sorted by ascending label AFTER limiting to max bins by descending count," +
        " otherwise bins are sorted by descending count. Default is false."
    )
  private val getAggregateDescription = "Return aggregated bins for this search query and a given field."

  private val getAggregateEndpointNoAuth =
    insecureEndpoint
      .errorOut(
        oneOf[HttpError](
          badRequestUnparsableQuery
        )
      )
      .get
      .in("aggregate" + noAuthSuffix)
      .in(queryInput)
      .in(titleInput)
      .in(authorsInput)
      .in(authorIncludeInput)
      .in(strictInput)
      .in(fromYearInput)
      .in(toYearInput)
      .in(docRefsInput)
      .in(aggregateFieldInput)
      .in(maxBinsInput)
      .in(sortByLabelInput)
      .in(clientIp)
      .out(jsonBody[AggregationBins].example(SearchHelper.aggregationBinsExample))
      .description(getAggregateDescription + noAuthDescription)

  private val getAggregateHttpNoAuth: ZServerEndpoint[Requirements, Any] =
    getAggregateEndpointNoAuth.zServerLogic[Requirements](input => getAggregateLogicNoAuth.tupled(input))

  private val getAggregateEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        Option[String],
        Option[String],
        List[String],
        Option[Boolean],
        Option[Boolean],
        Option[Int],
        Option[Int],
        List[String],
        String,
        Option[Int],
        Option[Boolean],
        Option[String]
    ),
    HttpError,
    AggregationBins,
    Any
  ] =
    secureEndpoint()
      .errorOutVariantsPrepend(badRequestUnparsableQuery)
      .get
      .in("aggregate")
      .in(queryInput)
      .in(titleInput)
      .in(authorsInput)
      .in(authorIncludeInput)
      .in(strictInput)
      .in(fromYearInput)
      .in(toYearInput)
      .in(docRefsInput)
      .in(aggregateFieldInput)
      .in(maxBinsInput)
      .in(sortByLabelInput)
      .in(clientIp)
      .out(jsonBody[AggregationBins].example(SearchHelper.aggregationBinsExample))
      .description(getAggregateDescription)

  private val getAggregateHttp: ZServerEndpoint[Requirements, Any] =
    getAggregateEndpoint.serverLogic[Requirements](token => input => getAggregateLogic.tupled(Tuple1(token) ++ input))

  private val badRequestOffsetHigherThanDocLength = oneOfVariant[BadRequest](
    StatusCode.BadRequest,
    jsonBody[BadRequest].description(
      "Offset higher than document length."
    )
  )

  private val wordOffsetInput = query[Int]("word-offset")
    .description("The start character offset of the word whose image or text we want")
    .example(10200)

  private val getWordImageDescription = "Return a word image in PNG format."

  private val getWordImageEndpointNoAuth =
    insecureEndpoint
      .errorOut(
        oneOf[HttpError](
          badRequestOffsetHigherThanDocLength,
          notFoundDocRef
        )
      )
      .get
      .in("word-image" + noAuthSuffix)
      .in(docReferenceInput)
      .in(wordOffsetInput)
      .in(clientIp)
      .out(header(Header.contentType(MediaType.ImagePng)))
      .out(streamBinaryBody(ZioStreams)(PngCodecFormat))
      .description(getWordImageDescription + noAuthDescription)

  private val getWordImageHttpNoAuth: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getWordImageEndpointNoAuth.zServerLogic[Requirements](input => getWordImageLogicNoAuth.tupled(input))

  private val getWordImageEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        DocReference,
        Int,
        Option[String]
    ),
    HttpError,
    ZStream[Any, Throwable, Byte],
    Any & ZioStreams
  ] =
    secureEndpoint()
      .errorOutVariantsPrepend(
        badRequestOffsetHigherThanDocLength,
        notFoundDocRef
      )
      .get
      .in("word-image")
      .in(docReferenceInput)
      .in(wordOffsetInput)
      .in(clientIp)
      .out(header(Header.contentType(MediaType.ImagePng)))
      .out(streamBinaryBody(ZioStreams)(PngCodecFormat))
      .description(getWordImageDescription)

  private val getWordImageHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getWordImageEndpoint.serverLogic[Requirements](token => input => getWordImageLogic.tupled(Tuple1(token) ++ input))

  private val getWordTextDescription = "Return a word text."

  private val getWordTextEndpointNoAuth =
    insecureEndpoint
      .errorOut(
        oneOf[HttpError](
          badRequestOffsetHigherThanDocLength,
          notFoundDocRef
        )
      )
      .get
      .in("word-text" + noAuthSuffix)
      .in(docReferenceInput)
      .in(wordOffsetInput)
      .in(clientIp)
      .out(jsonBody[WordText].example(WordText("יום־טוב")))
      .description(getWordTextDescription + noAuthDescription)

  private val getWordTextHttpNoAuth: ZServerEndpoint[Requirements, Any] =
    getWordTextEndpointNoAuth.zServerLogic[Requirements](input => getWordTextLogicNoAuth.tupled(input))

  private val getWordTextEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        DocReference,
        Int,
        Option[String]
    ),
    HttpError,
    WordText,
    Any
  ] =
    secureEndpoint()
      .errorOutVariantsPrepend(
        badRequestOffsetHigherThanDocLength,
        notFoundDocRef
      )
      .get
      .in("word-text")
      .in(docReferenceInput)
      .in(wordOffsetInput)
      .in(clientIp)
      .out(jsonBody[WordText].example(WordText("יום־טוב")))
      .description(getWordTextDescription)

  private val getWordTextHttp: ZServerEndpoint[Requirements, Any] =
    getWordTextEndpoint.serverLogic[Requirements](token => input => getWordTextLogic.tupled(Tuple1(token) ++ input))

  private val authorPrefixInput = query[String]("prefix").description("The author name prefix").example("ש")
  private val includeAuthorFieldInput = query[Option[Boolean]]("includeAuthor")
    .description("If true, the Author field is included. Defaults to true.")
    .example(Some(true))

  private val includeAuthorInTranscriptionFieldInput = query[Option[Boolean]]("includeAuthorInTranscription")
    .description("If true, the AuthorEnglish field is included. Defaults to true.")
    .example(Some(true))

  private val getTopAuthorsDescription = "Return most common authors matching prefix in alphabetical order."

  private val getTopAuthorsEndpointNoAuth =
    insecureEndpoint
      .errorOut(oneOf[HttpError](badRequestUnparsableQuery))
      .get
      .in("authors" + noAuthSuffix)
      .in(authorPrefixInput)
      .in(maxBinsInput)
      .in(includeAuthorFieldInput)
      .in(includeAuthorInTranscriptionFieldInput)
      .in(clientIp)
      .out(jsonBody[AggregationBins].example(SearchHelper.aggregationBinsExample))
      .description(getTopAuthorsDescription + noAuthDescription)

  private val getTopAuthorsHttpNoAuth: ZServerEndpoint[Requirements, Any] =
    getTopAuthorsEndpointNoAuth.zServerLogic[Requirements](input => getTopAuthorsLogicNoAuth.tupled(input))

  private val getTopAuthorsEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        String,
        Option[Int],
        Option[Boolean],
        Option[Boolean],
        Option[String]
    ),
    HttpError,
    AggregationBins,
    Any
  ] =
    secureEndpoint()
      .errorOutVariantsPrepend(badRequestUnparsableQuery)
      .get
      .in("authors")
      .in(authorPrefixInput)
      .in(maxBinsInput)
      .in(includeAuthorFieldInput)
      .in(includeAuthorInTranscriptionFieldInput)
      .in(clientIp)
      .out(jsonBody[AggregationBins].example(SearchHelper.aggregationBinsExample))
      .description(getTopAuthorsDescription)

  private val getTopAuthorsHttp: ZServerEndpoint[Requirements, Any] =
    getTopAuthorsEndpoint.serverLogic[Requirements](token => input => getTopAuthorsLogic.tupled(Tuple1(token) ++ input))

  private val textAsHtmlInput = query[Option[Boolean]]("text-as-html")
    .description("Should the text be pre-formatted as HTML. Defaults to false.")
    .example(Some(true))

  private val getHighlightedTextDescription =
    "Return the highlighted document. Note that a highlight can contain a newline character, if it concerns a hyphenated word."

  private val getHighlightedTextEndpointNoAuth =
    insecureEndpoint.get
      .errorOut(oneOf[HttpError](notFoundDocRef))
      .in("highlighted-text" + noAuthSuffix)
      .in(docReferenceInput)
      .in(queryInput)
      .in(strictInput)
      .in(textAsHtmlInput)
      .in(clientIp)
      .out(jsonBody[HighlightedDocument].example(SearchHelper.highlightedDocExample))
      .description(getHighlightedTextDescription + noAuthDescription)

  private val getHighlightedTextHttpNoAuth: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getHighlightedTextEndpointNoAuth.zServerLogic[Requirements](input => getHighlightedTextLogicNoAuth.tupled(input))

  private val getHighlightedTextEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        DocReference,
        Option[String],
        Option[Boolean],
        Option[Boolean],
        Option[String]
    ),
    HttpError,
    HighlightedDocument,
    Any
  ] =
    secureEndpoint().get
      .errorOutVariantsPrepend(notFoundDocRef)
      .in("highlighted-text")
      .in(docReferenceInput)
      .in(queryInput)
      .in(strictInput)
      .in(textAsHtmlInput)
      .in(clientIp)
      .out(jsonBody[HighlightedDocument].example(SearchHelper.highlightedDocExample))
      .description(getHighlightedTextDescription)

  private val getHighlightedTextHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getHighlightedTextEndpoint.serverLogic[Requirements](token =>
      input => getHighlightedTextLogic.tupled(Tuple1(token) ++ input)
    )

  private val dehyphenateInput = query[Option[Boolean]]("dehyphenate")
    .description(
      "Should the text be dehyphenated, with physical newlines removed and end-of-line hyphenated words dehyphenated if required." +
        " Defaults to false."
    )
    .example(Some(true))

  private val getTextDescription = "Return the document in plain text format, with possible de-hyphenation."

  private val getTextEndpointNoAuth =
    insecureEndpoint.get
      .errorOut(oneOf[HttpError](notFoundDocRef))
      .in("text" + noAuthSuffix)
      .in(docReferenceInput)
      .in(dehyphenateInput)
      .in(clientIp)
      .out(header[String]("Content-Disposition"))
      .out(
        streamTextBody(ZioStreams)(
          CodecFormat.TextPlain(),
          Some(StandardCharsets.UTF_8)
        )
      )
      .description(getTextDescription + noAuthDescription)

  private val getTextHttpNoAuth: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getTextEndpointNoAuth.zServerLogic[Requirements](input => getTextLogicNoAuth.tupled(input))

  private val getTextEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        DocReference,
        Option[Boolean],
        Option[String]
    ),
    HttpError,
    (String, ZStream[Any, Throwable, Byte]),
    Any & ZioStreams
  ] =
    secureEndpoint().get
      .errorOutVariantsPrepend(notFoundDocRef)
      .in("text")
      .in(docReferenceInput)
      .in(dehyphenateInput)
      .in(clientIp)
      .out(header[String]("Content-Disposition"))
      .out(
        streamTextBody(ZioStreams)(
          CodecFormat.TextPlain(),
          Some(StandardCharsets.UTF_8)
        )
      )
      .description(getTextDescription)

  private val getTextHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getTextEndpoint.serverLogic[Requirements](token => input => getTextLogic.tupled(Tuple1(token) ++ input))

  private val simplifyTextInput = query[Option[Boolean]]("simplify-text")
    .description(
      "Whether text should be simplified in term of unicode representation and unexpected diacritics. Default is false."
    )
    .example(Some(true))

  private val getTextAsHtmlDescription = "Return the document in HTML format."

  private val getTextAsHtmlEndpointNoAuth =
    insecureEndpoint.get
      .errorOut(oneOf[HttpError](badRequestUnparsableQuery, notFoundDocRef))
      .in("text-as-html" + noAuthSuffix)
      .in(docReferenceInput)
      .in(queryInput)
      .in(strictInput)
      .in(simplifyTextInput)
      .in(clientIp)
      .out(header[String]("Content-Disposition"))
      .out(
        streamTextBody(ZioStreams)(
          CodecFormat.TextHtml(),
          Some(StandardCharsets.UTF_8)
        )
      )
      .description(getTextAsHtmlDescription + noAuthDescription)

  private val getTextAsHtmlHttpNoAuth: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getTextAsHtmlEndpointNoAuth.zServerLogic[Requirements](input => getTextAsHtmlLogicNoAuth.tupled(input))

  private val getTextAsHtmlEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        DocReference,
        Option[String],
        Option[Boolean],
        Option[Boolean],
        Option[String]
    ),
    HttpError,
    (String, ZStream[Any, Throwable, Byte]),
    Any & ZioStreams
  ] =
    secureEndpoint().get
      .errorOutVariantsPrepend(badRequestUnparsableQuery, notFoundDocRef)
      .in("text-as-html")
      .in(docReferenceInput)
      .in(queryInput)
      .in(strictInput)
      .in(simplifyTextInput)
      .in(clientIp)
      .out(header[String]("Content-Disposition"))
      .out(
        streamTextBody(ZioStreams)(
          CodecFormat.TextHtml(),
          Some(StandardCharsets.UTF_8)
        )
      )
      .description(getTextAsHtmlDescription)

  private val getTextAsHtmlHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    getTextAsHtmlEndpoint.serverLogic[Requirements](token => input => getTextAsHtmlLogic.tupled(Tuple1(token) ++ input))

  private val getListDescription = "List documents in the OCR index."

  private val getListEndpointNoAuth =
    insecureEndpoint
      .errorOut(oneOf[HttpError](badRequestUnparsableQuery))
      .get
      .in("list" + noAuthSuffix)
      .in(queryInput)
      .in(titleInput)
      .in(authorsInput)
      .in(authorIncludeInput)
      .in(strictInput)
      .in(fromYearInput)
      .in(toYearInput)
      .in(docRefsInput)
      .in(ocrSoftwareInput)
      .in(sortInput)
      .in(clientIp)
      .out(jsonBody[Seq[DocReference]].example(Seq(DocReference("nybc200089"), DocReference("nybc212100"))))
      .description(getListDescription + noAuthDescription)

  private val getListHttpNoAuth: ZServerEndpoint[Requirements, Any] =
    getListEndpointNoAuth.zServerLogic[Requirements](input => getListLogicNoAuth.tupled(input))

  private val getListEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        Option[String],
        Option[String],
        List[String],
        Option[Boolean],
        Option[Boolean],
        Option[Int],
        Option[Int],
        List[String],
        Option[String],
        Option[String],
        Option[String]
    ),
    HttpError,
    Seq[DocReference],
    Any
  ] =
    secureEndpoint()
      .errorOutVariantsPrepend(badRequestUnparsableQuery)
      .get
      .in("list")
      .in(queryInput)
      .in(titleInput)
      .in(authorsInput)
      .in(authorIncludeInput)
      .in(strictInput)
      .in(fromYearInput)
      .in(toYearInput)
      .in(docRefsInput)
      .in(ocrSoftwareInput)
      .in(sortInput)
      .in(clientIp)
      .out(jsonBody[Seq[DocReference]].example(Seq(DocReference("nybc200089"), DocReference("nybc212100"))))
      .description(getListDescription)

  private val getListHttp: ZServerEndpoint[Requirements, Any] =
    getListEndpoint.serverLogic[Requirements](token => input => getListLogic.tupled(Tuple1(token) ++ input))

  private val getSizeEndpoint =
    insecureEndpoint.get
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[NotFound](StatusCode.NotFound, jsonBody[NotFound].description("Index not found"))
        )
      )
      .in("size")
      .out(jsonBody[SizeResponse].example(SizeResponse(42)))
      .description("Return the number of documents in the index")

  private val getSizeHttp: ZServerEndpoint[Requirements, Any] =
    getSizeEndpoint.zServerLogic[Requirements](_ => getSizeLogic())

  private val httpNoAuth: List[ZServerEndpoint[Requirements, Any & ZioStreams]] = List(
    getSearchHttpNoAuth,
    getImageSnippetHttpNoAuth,
    getImageSnippetWithHighlightsHttpNoAuth,
    getWordTextHttpNoAuth,
    getWordImageHttpNoAuth,
    getAggregateHttpNoAuth,
    getTopAuthorsHttpNoAuth,
    getHighlightedTextHttpNoAuth,
    getTextHttpNoAuth,
    getTextAsHtmlHttpNoAuth,
    getListHttpNoAuth,
    getSizeHttp
  )

  private val httpAuth: List[ZServerEndpoint[Requirements, Any & ZioStreams]] = List(
    getSearchHttp,
    getImageSnippetHttp,
    getImageSnippetWithHighlightsHttp,
    getWordTextHttp,
    getWordImageHttp,
    getAggregateHttp,
    getTopAuthorsHttp,
    getHighlightedTextHttp,
    getTextHttp,
    getTextAsHtmlHttp,
    getListHttp
  )

  val http: List[ZServerEndpoint[Requirements, Any & ZioStreams]] =
    httpAuth ++ (if (allowSearchWithoutAuth) { httpNoAuth }
                 else { List() })

  val endpoints: List[AnyEndpoint] =
    httpAuth.map(_.endpoint.tag("search")) ++ (if (allowSearchWithoutAuth) {
                                                 httpNoAuth.map(_.endpoint.tag("search-no-auth"))
                                               } else { List() })
}
