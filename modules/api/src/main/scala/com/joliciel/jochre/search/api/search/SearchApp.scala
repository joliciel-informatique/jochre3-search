package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.api.HttpError.{BadRequest, NotFound}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.{AuthenticationProvider, TokenAuthentication, ValidToken}
import com.joliciel.jochre.search.api.{HttpError, PngCodecFormat}
import com.joliciel.jochre.search.core.service.{Highlight, SearchHelper, SearchProtocol, SearchResponse}
import com.joliciel.jochre.search.core.{AggregationBins, DocReference, IndexField}
import io.circe.generic.auto._
import shapeless.syntax.std.tuple._
import sttp.capabilities.zio.ZioStreams
import sttp.model.{Header, MediaType, StatusCode}
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.ztapir._
import sttp.tapir.{AnyEndpoint, CodecFormat}

import java.nio.charset.StandardCharsets
import scala.concurrent.ExecutionContext

case class SearchApp(override val authenticationProvider: AuthenticationProvider, executionContext: ExecutionContext)
    extends TokenAuthentication
    with SearchLogic
    with SearchProtocol
    with SearchSchemaSupport {
  implicit val ec: ExecutionContext = executionContext

  private val queryInput = query[Option[String]]("query")
    .description("Query string for searching in the text")
    .example(Some(""""פון * װעגן""""))
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

  private val getSearchEndpoint =
    insecureEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparseable query"))
        )
      )
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
      .in(clientIp)
      .out(jsonBody[SearchResponse].example(SearchHelper.searchResponseExample))
      .description("Search the OCR index.")

  private val getSearchHttp: ZServerEndpoint[Requirements, Any] =
    getSearchEndpoint.zServerLogic[Requirements](input => (getSearchLogic _).tupled(input))

  private val getSearchWithAuthEndpoint: ZPartialServerEndpoint[
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
        Option[String]
    ),
    HttpError,
    SearchResponse,
    Any
  ] =
    secureEndpoint()
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparseable query"))
      )
      .get
      .in("search-with-auth")
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
      .in(clientIp)
      .out(jsonBody[SearchResponse].example(SearchHelper.searchResponseExample))
      .description("Search the OCR index for an authenticated user - will store the search against the username.")

  private val getSearchWithAuthHttp: ZServerEndpoint[Requirements, Any] =
    getSearchWithAuthEndpoint.serverLogic[Requirements](token =>
      input => (getSearchWithAuthLogic _).tupled(token +: input)
    )

  private val getImageSnippetEndpoint =
    insecureEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](
            StatusCode.BadRequest,
            jsonBody[BadRequest].description(
              "Offsets refer to words on different pages, or are higher than document length."
            )
          ),
          oneOfVariant[NotFound](
            StatusCode.NotFound,
            jsonBody[NotFound].description(
              "Requested document reference not found in index."
            )
          )
        )
      )
      .get
      .in("image-snippet")
      .in(
        query[DocReference]("doc-ref")
          .description("The document containing the image")
          .example(DocReference("nybc200089"))
      )
      .in(
        query[Int]("start-offset")
          .description("The start character offset of the first word with respect to the entire document (inclusive)")
          .example(10200)
      )
      .in(
        query[Int]("end-offset")
          .description("The end character offset of the last word with respect to the entire document (exclusive)")
          .example(10450)
      )
      .in(
        query[List[Highlight]]("highlight")
          .description(
            "A list of words to highlight using the start index and end index of each word or contiguous list of words" +
              ", e.g. \"[10210,10215],[10312,10320]\""
          )
      )
      .out(header(Header.contentType(MediaType.ImagePng)))
      .out(streamBinaryBody(ZioStreams)(PngCodecFormat))
      .description("Return an image snippet in PNG format")

  private val getImageSnippetHttp: ZServerEndpoint[Requirements, Any with ZioStreams] =
    getImageSnippetEndpoint.zServerLogic[Requirements](input => (getImageSnippetLogic _).tupled(input))

  private val getAggregateEndpoint =
    insecureEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparseable query"))
        )
      )
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
      .in(
        query[String]("field")
          .description(f"The field to choose among ${IndexField.aggregatableFields.map(_.entryName).mkString(", ")}")
          .example(IndexField.Author.entryName)
      )
      .in(query[Int]("maxBins").description("Maximum bins to return").example(20))
      .out(jsonBody[AggregationBins].example(SearchHelper.aggregationBinsExample))
      .description("Return aggregated bins for this search query and a given field.")

  private val getAggregateHttp: ZServerEndpoint[Requirements, Any] =
    getAggregateEndpoint.zServerLogic[Requirements](input => (getAggregateLogic _).tupled(input))

  private val getWordImageEndpoint =
    insecureEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](
            StatusCode.BadRequest,
            jsonBody[BadRequest].description(
              "Offset higher than document length."
            )
          ),
          oneOfVariant[NotFound](
            StatusCode.NotFound,
            jsonBody[NotFound].description(
              "Requested document reference not found in index."
            )
          )
        )
      )
      .get
      .in("word-image")
      .in(
        query[DocReference]("doc-ref")
          .description("The document containing the image")
          .example(DocReference("nybc200089"))
      )
      .in(
        query[Int]("word-offset")
          .description("The start character offset of the word whose image we want")
          .example(10200)
      )
      .out(header(Header.contentType(MediaType.ImagePng)))
      .out(streamBinaryBody(ZioStreams)(PngCodecFormat))
      .description("Return a word image in PNG format")

  private val getWordImageHttp: ZServerEndpoint[Requirements, Any with ZioStreams] =
    getWordImageEndpoint.zServerLogic[Requirements](input => (getWordImageLogic _).tupled(input))

  private val getWordTextEndpoint =
    insecureEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](
            StatusCode.BadRequest,
            jsonBody[BadRequest].description(
              "Offset higher than document length."
            )
          ),
          oneOfVariant[NotFound](
            StatusCode.NotFound,
            jsonBody[NotFound].description(
              "Requested document reference not found in index."
            )
          )
        )
      )
      .get
      .in("word-text")
      .in(
        query[DocReference]("doc-ref")
          .description("The document containing the image")
          .example(DocReference("nybc200089"))
      )
      .in(
        query[Int]("word-offset")
          .description("The start character offset of the word whose image we want")
          .example(10200)
      )
      .out(jsonBody[WordText].example(WordText("יום־טוב")))
      .description("Return a word text")

  private val getWordTextHttp: ZServerEndpoint[Requirements, Any] =
    getWordTextEndpoint.zServerLogic[Requirements](input => (getWordTextLogic _).tupled(input))

  private val getTopAuthorsEndpoint =
    insecureEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparseable query"))
        )
      )
      .get
      .in("authors")
      .in(query[String]("prefix").description("The author name prefix").example("ש"))
      .in(query[Int]("maxBins").description("Maximum bins to return").example(20))
      .in(
        query[Option[Boolean]]("includeAuthor")
          .description("If true, the Author field is included. Defaults to true.")
          .example(Some(true))
      )
      .in(
        query[Option[Boolean]]("includeAuthorInTranscription")
          .description("If true, the AuthorEnglish field is included. Defaults to true.")
          .example(Some(true))
      )
      .out(jsonBody[AggregationBins].example(SearchHelper.aggregationBinsExample))
      .description("Return most common authors matching prefix in alphabetical order.")

  private val getTopAuthorsHttp: ZServerEndpoint[Requirements, Any] =
    getTopAuthorsEndpoint.zServerLogic[Requirements](input => (getTopAuthorsLogic _).tupled(input))

  private val getTextAsHtmlEndpoint =
    insecureEndpoint.get
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[NotFound](
            StatusCode.NotFound,
            jsonBody[NotFound].description("Document reference not found in index")
          )
        )
      )
      .in("text-as-html")
      .in(
        query[DocReference]("doc-ref")
          .description("Document reference whose text we want.")
          .example(DocReference("nybc200089"))
      )
      .out(
        streamTextBody(ZioStreams)(
          CodecFormat.TextHtml(),
          Some(StandardCharsets.UTF_8)
        )
      )
      .description("Return the document in HTML format")

  private val getTextAsHtmlHttp: ZServerEndpoint[Requirements, Any with ZioStreams] =
    getTextAsHtmlEndpoint.zServerLogic[Requirements](input => getTextAsHtmlLogic(input))

  private val getListEndpoint =
    insecureEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparseable query"))
        )
      )
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
      .description("List documents in the OCR index.")

  private val getListHttp: ZServerEndpoint[Requirements, Any] =
    getListEndpoint.zServerLogic[Requirements](input => (getListLogic _).tupled(input))

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

  val endpoints: List[AnyEndpoint] =
    List(
      getSearchWithAuthHttp
    ).map(_.endpoint.tag("search")) ++
      List(
        getSearchEndpoint,
        getImageSnippetEndpoint,
        getAggregateEndpoint,
        getTopAuthorsEndpoint,
        getTextAsHtmlEndpoint,
        getListEndpoint,
        getSizeEndpoint,
        getWordTextEndpoint,
        getWordImageEndpoint
      ).map(_.tag("search"))

  val http: List[ZServerEndpoint[Requirements, Any with ZioStreams]] = List(
    getSearchWithAuthHttp,
    getSearchHttp,
    getImageSnippetHttp,
    getAggregateHttp,
    getTopAuthorsHttp,
    getTextAsHtmlHttp,
    getListHttp,
    getSizeHttp,
    getWordTextHttp,
    getWordImageHttp
  )
}
