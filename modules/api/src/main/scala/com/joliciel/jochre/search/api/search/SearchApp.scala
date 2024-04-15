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
import zio.stream.ZStream

import java.nio.charset.StandardCharsets
import scala.concurrent.ExecutionContext

case class SearchApp(override val authenticationProvider: AuthenticationProvider, executionContext: ExecutionContext)
    extends TokenAuthentication
    with SearchLogic
    with SearchProtocol
    with SearchSchemaSupport {
  implicit val ec: ExecutionContext = executionContext

  val getSearchEndpoint: ZPartialServerEndpoint[
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
        Option[String]
    ),
    HttpError,
    SearchResponse,
    Any
  ] =
    secureEndpoint()
      .errorOutVariant[HttpError](
        oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparseable query"))
      )
      .get
      .in("search")
      .in(
        query[Option[String]]("query")
          .description("Query string for searching in the text")
          .example(Some(""""פון * װעגן""""))
      )
      .in(query[Option[String]]("title").description("Query string for searching in the title").example(Some("מאָטעל")))
      .in(query[List[String]]("authors").description("Authors to include or exclude").example(List("שלום עליכם")))
      .in(
        query[Option[Boolean]]("author-include")
          .description("Whether the authors should be included or excluded. Default is true.")
          .example(Some(true))
      )
      .in(
        query[Option[Boolean]]("strict")
          .description(
            "Whether query strings should be expanded to related synonyms (false) or not (true). Default is false."
          )
          .example(Some(false))
      )
      .in(query[Option[Int]]("from-year").description("The earliest year of publication").example(Some(1900)))
      .in(query[Option[Int]]("to-year").description("The latest year of publication").example(Some(1920)))
      .in(
        query[List[String]]("doc-refs").description("Which document references to include").example(List("nybc200089"))
      )
      .in(query[Int]("first").description("The first result to return on the page of results").example(20))
      .in(query[Int]("max").description("The max number of results to return on the page of results").example(10))
      .in(query[Option[Int]]("max-snippets").description("The maximum number of snippets per result").example(Some(20)))
      .in(
        query[Option[Int]]("row-padding")
          .description("How many rows to add in the snippet before the first highlight and after the last highlight")
          .example(Some(2))
      )
      .in(query[Option[String]]("sort").description("The sort order (optional)").example(None))
      .out(jsonBody[SearchResponse].example(SearchHelper.searchResponseExample))
      .description("Search the OCR index.")

  val getSearchHttp: ZServerEndpoint[Requirements, Any] =
    getSearchEndpoint.serverLogic[Requirements](token => input => (getSearchLogic _).tupled(token +: input))

  val getImageSnippetEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (DocReference, Int, Int, List[Highlight]),
    HttpError,
    ZStream[Any, Throwable, Byte],
    Any with ZioStreams
  ] =
    secureEndpoint()
      .errorOutVariant[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description(
            "Offsets refer to words on different pages, or are higher than document length."
          )
        )
      )
      .errorOutVariant[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Requested document reference not found in index."
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

  val getImageSnippetHttp: ZServerEndpoint[Requirements, Any with ZioStreams] =
    getImageSnippetEndpoint.serverLogic[Requirements](token => input => (getImageSnippetLogic _).tupled(token +: input))

  val getAggregateEndpoint: ZPartialServerEndpoint[
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
        Int
    ),
    HttpError,
    AggregationBins,
    Any
  ] =
    secureEndpoint()
      .errorOutVariant[HttpError](
        oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparseable query"))
      )
      .get
      .in("aggregate")
      .in(
        query[Option[String]]("query")
          .description("Query string for searching in the text")
          .example(Some(""""פון * װעגן""""))
      )
      .in(query[Option[String]]("title").description("Query string for searching in the title").example(Some("מאָטעל")))
      .in(query[List[String]]("authors").description("Authors to include or exclude").example(List("שלום עליכם")))
      .in(
        query[Option[Boolean]]("authorInclude")
          .description("Whether the authors should be included or excluded. Default is true.")
          .example(Some(true))
      )
      .in(
        query[Option[Boolean]]("strict")
          .description(
            "Whether query strings should be expanded to related synonyms (false) or not (true). Default is false."
          )
          .example(Some(false))
      )
      .in(query[Option[Int]]("fromYear").description("The earliest year of publication").example(Some(1900)))
      .in(query[Option[Int]]("toYear").description("The latest year of publication").example(Some(1920)))
      .in(
        query[List[String]]("docRefs").description("Which document references to include").example(List("nybc200089"))
      )
      .in(
        query[String]("field")
          .description(f"The field to choose among ${IndexField.aggregatableFields.map(_.entryName).mkString(", ")}")
          .example(IndexField.Author.entryName)
      )
      .in(query[Int]("maxBins").description("Maximum bins to return").example(20))
      .out(jsonBody[AggregationBins].example(SearchHelper.aggregationBinsExample))
      .description("Return aggregated bins for this search query and a given field.")

  val getAggregateHttp: ZServerEndpoint[Requirements, Any] =
    getAggregateEndpoint.serverLogic[Requirements](token => input => (getAggregateLogic _).tupled(token +: input))

  val getTopAuthorsEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (String, Int),
    HttpError,
    AggregationBins,
    Any
  ] =
    secureEndpoint()
      .errorOutVariant[HttpError](
        oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparseable query"))
      )
      .get
      .in("authors")
      .in(query[String]("prefix").description("The author name prefix").example("ש"))
      .in(query[Int]("maxBins").description("Maximum bins to return").example(20))
      .out(jsonBody[AggregationBins].example(SearchHelper.aggregationBinsExample))
      .description("Return most common authors matching prefix in alphabetical order.")

  val getTopAuthorsHttp: ZServerEndpoint[Requirements, Any] =
    getTopAuthorsEndpoint.serverLogic[Requirements](token => input => (getTopAuthorsLogic _).tupled(token +: input))

  val getTextAsHtmlEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    DocReference,
    HttpError,
    ZStream[
      Any,
      Throwable,
      Byte
    ],
    Any with ZioStreams
  ] = secureEndpoint().get
    .errorOutVariant[HttpError](
      oneOfVariant[NotFound](
        StatusCode.NotFound,
        jsonBody[NotFound].description("Document reference not found in index")
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

  val getTextAsHtmlHttp: ZServerEndpoint[Requirements, Any with ZioStreams] =
    getTextAsHtmlEndpoint.serverLogic[Requirements](_ => input => getTextAsHtmlLogic(input))

  val getSizeEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    Unit,
    HttpError,
    SizeResponse,
    Any
  ] = secureEndpoint().get
    .errorOutVariant[HttpError](
      oneOfVariant[NotFound](StatusCode.NotFound, jsonBody[NotFound].description("Index not found"))
    )
    .in("size")
    .out(jsonBody[SizeResponse].example(SizeResponse(42)))
    .description("Return the number of documents in the index")

  val getSizeHttp: ZServerEndpoint[Requirements, Any] =
    getSizeEndpoint.serverLogic[Requirements](_ => _ => getSizeLogic())

  val endpoints: List[AnyEndpoint] = List(
    getSearchEndpoint,
    getImageSnippetEndpoint,
    getAggregateEndpoint,
    getTopAuthorsEndpoint,
    getTextAsHtmlEndpoint,
    getSizeEndpoint
  ).map(_.endpoint.tag("search"))

  val http: List[ZServerEndpoint[Requirements, Any with ZioStreams]] = List(
    getSearchHttp,
    getImageSnippetHttp,
    getAggregateHttp,
    getTopAuthorsHttp,
    getTextAsHtmlHttp,
    getSizeHttp
  )
}
