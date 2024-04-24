package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.api.HttpError.{BadRequest, NotFound}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.{AuthenticationProvider, TokenAuthentication, ValidToken}
import com.joliciel.jochre.search.api.{HttpError, OkResponse, Roles}
import com.joliciel.jochre.search.core.{CoreProtocol, DocReference, MetadataField}
import io.circe.generic.auto._
import sttp.capabilities.zio.ZioStreams
import sttp.model.StatusCode
import sttp.tapir.AnyEndpoint
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.ztapir._
import shapeless.syntax.std.tuple._

import scala.concurrent.ExecutionContext

case class IndexApp(override val authenticationProvider: AuthenticationProvider, executionContext: ExecutionContext)
    extends TokenAuthentication
    with IndexLogic
    with IndexSchemaSupport
    with CoreProtocol {
  implicit val ec: ExecutionContext = executionContext

  private val postIndexPdfEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (PdfFileForm, Option[String]),
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
      .in(clientIp)
      .out(jsonBody[IndexResponse].example(IndexHelper.indexResponseExample))
      .description("Post an image file for analysis and return xml result.")

  private val postIndexHttp: ZServerEndpoint[Requirements, Any] =
    postIndexPdfEndpoint.serverLogic[Requirements](token => input => (postIndexPdfLogic _).tupled(token +: input))

  private val postWordSuggestionEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (WordSuggestionForm, Option[String]),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint()
      .errorOutVariant[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Document reference not found"
          )
        )
      )
      .post
      .in("suggest-word")
      .in(
        jsonBody[WordSuggestionForm]
      )
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description("Make a suggestion for a given word that was OCR'd incorrectly.")

  private val postWordSuggestionHttp: ZServerEndpoint[Requirements, Any] =
    postWordSuggestionEndpoint.serverLogic[Requirements](token =>
      input => (postWordSuggestionLogic _).tupled(token +: input)
    )

  private val postMetadataCorrectionEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (MetadataCorrectionForm, Option[String]),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint()
      .errorOutVariant[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Document reference not found"
          )
        )
      )
      .post
      .in("correct-metadata")
      .in(
        jsonBody[MetadataCorrectionForm].example(
          MetadataCorrectionForm(
            DocReference("nybc200089"),
            MetadataField.Author.entryName,
            "שלום עליכם",
            applyEverywhere = false
          )
        )
      )
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description(
        f"Correct metadata for a given document reference. Field is one of ${MetadataField.values.map(_.entryName).mkString(", ")}"
      )

  private val postMetadataCorrectionHttp: ZServerEndpoint[Requirements, Any] =
    postMetadataCorrectionEndpoint.serverLogic[Requirements](token =>
      input => (postMetadataCorrectionLogic _).tupled(token +: input)
    )

  val endpoints: List[AnyEndpoint] = List(
    postIndexPdfEndpoint,
    postWordSuggestionEndpoint,
    postMetadataCorrectionEndpoint
  ).map(_.endpoint.tag("index"))

  val http: List[ZServerEndpoint[Requirements, Any with ZioStreams]] = List(
    postIndexHttp,
    postWordSuggestionHttp,
    postMetadataCorrectionHttp
  )
}
