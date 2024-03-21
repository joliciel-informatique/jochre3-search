package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.api.HttpError.BadRequest
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.{AuthenticationProvider, TokenAuthentication, ValidToken}
import com.joliciel.jochre.search.api.{HttpError, Roles}
import io.circe.generic.auto._
import sttp.capabilities.zio.ZioStreams
import sttp.model.StatusCode
import sttp.tapir.AnyEndpoint
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.ztapir._

import scala.concurrent.ExecutionContext

case class IndexApp(override val authenticationProvider: AuthenticationProvider, executionContext: ExecutionContext)
    extends TokenAuthentication
    with IndexLogic {
  implicit val ec: ExecutionContext = executionContext

  val postIndexPdfEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    PdfFileForm,
    HttpError,
    IndexResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariant[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description(
            "Pdf file is not a valid pdf, or Alto file is not a zip file containing Alto XML"
          )
        )
      )
      .post
      .in("index")
      .in("pdf")
      .in(
        multipartBody[PdfFileForm]
      )
      .out(jsonBody[IndexResponse].example(IndexHelper.indexResponseExample))
      .description("Post an image file for analysis and return xml result.")

  val postIndexHttp: ZServerEndpoint[Requirements, Any] =
    postIndexPdfEndpoint.serverLogic[Requirements](token => input => postIndexPdfLogic(token, input))

  val endpoints: List[AnyEndpoint] = List(
    postIndexPdfEndpoint
  ).map(_.endpoint.tag("index"))

  val http: List[ZServerEndpoint[Requirements, Any with ZioStreams]] = List(
    postIndexHttp
  )
}
