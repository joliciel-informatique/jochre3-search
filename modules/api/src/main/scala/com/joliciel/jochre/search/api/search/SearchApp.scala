package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.api.{HttpError, PngCodecFormat}
import com.joliciel.jochre.search.api.HttpError.{BadRequest, NotFound}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.{AuthenticationProvider, TokenAuthentication, ValidToken}
import sttp.model.{Header, HeaderNames, MediaType, StatusCode}
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._
import io.circe.generic.auto._
import sttp.tapir.ztapir._
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.ztapir.{ZPartialServerEndpoint, ZServerEndpoint, oneOfVariant, path}
import shapeless.syntax.std.tuple._
import sttp.capabilities.zio.ZioStreams
import sttp.tapir.{AnyEndpoint, CodecFormat}
import zio.stream.ZStream

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
    (String, Int, Int, Option[Int], Option[Int]),
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
      .in(query[String]("query").description("The search query string").example(""""פון * װעגן""""))
      .in(query[Int]("first").description("The first result to return on the page of results").example(20))
      .in(query[Int]("max").description("The max number of results to return on the page of results").example(10))
      .in(query[Option[Int]]("max-snippets").description("The maximum number of snippets per result").example(Some(20)))
      .in(
        query[Option[Int]]("row-padding")
          .description("How many rows to add in the snippet before the first highlight and after the last highlight")
          .example(Some(2))
      )
      .out(jsonBody[SearchResponse].example(SearchHelper.searchResponseExample))
      .description("Search the OCR index.")

  val getSearchHttp: ZServerEndpoint[Requirements, Any] =
    getSearchEndpoint.serverLogic[Requirements](token => input => (getSearchLogic _).tupled(token +: input))

  val getImageSnippetEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (DocId, Int, Int, Int, List[Highlight]),
    HttpError,
    ZStream[Any, Throwable, Byte],
    Any with ZioStreams
  ] =
    secureEndpoint()
      .errorOutVariant[HttpError](
        oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparseable query"))
      )
      .get
      .in("image-snippet")
      .in(query[DocId]("doc-id").description("The document containing the image").example(DocId("nybc200089")))
      .in(query[Int]("page").description("The page number in the document").example(20))
      .in(query[Int]("start-line").description("The start line in the page (inclusive)").example(11))
      .in(query[Int]("end-line").description("The end line in the page (exclusive)").example(17))
      .in(
        query[List[Highlight]]("highlight")
          .description(
            "A list of words to highlight using the start index and end index of each word or contiguous list of words" +
              ", e.g. \"[100,120]\""
          )
      )
      .out(header(Header.contentType(MediaType.ImagePng)))
      .out(streamBinaryBody(ZioStreams)(PngCodecFormat))
      .description("Return an image snippet in PNG format")

  val getImageSnippetHttp: ZServerEndpoint[Requirements, Any with ZioStreams] =
    getImageSnippetEndpoint.serverLogic[Requirements](token => input => (getImageSnippetLogic _).tupled(token +: input))

  val endpoints: List[AnyEndpoint] = List(
    getSearchEndpoint,
    getImageSnippetEndpoint
  ).map(_.endpoint)

  val http: List[ZServerEndpoint[Requirements, Any with ZioStreams]] = List(
    getSearchHttp,
    getImageSnippetHttp
  )
}
