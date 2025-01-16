package com.joliciel.jochre.search.api.users

import com.joliciel.jochre.search.api.HttpError.{BadRequest, NotFound}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.{AuthenticationProvider, TokenAuthentication, ValidToken}
import com.joliciel.jochre.search.api.{HttpError, OkResponse, TapirSchemaSupport}
import com.joliciel.jochre.search.core.CoreProtocol
import io.circe.Json
import io.circe.generic.auto._
import io.circe.literal._
import sttp.capabilities.zio.ZioStreams
import sttp.model.StatusCode
import sttp.tapir.AnyEndpoint
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.ztapir._

import scala.concurrent.ExecutionContext

case class UserApp(override val authenticationProvider: AuthenticationProvider, executionContext: ExecutionContext)
    extends TokenAuthentication
    with UserLogic
    with TapirSchemaSupport
    with CoreProtocol {
  given ExecutionContext = executionContext

  val upsertPreferenceEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (String, Json),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint().post
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest].description("Unparseable json"))
      )
      .in("preferences")
      .in(path[String]("key").example("main"))
      .in(
        jsonBody[Json]
          .description("Json to insert for preferences")
          .example(json"""{"language":"yi"}""")
      )
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description("Insert or update user preferences for a particular key.")

  val upsertPreferenceHttp: ZServerEndpoint[Requirements, Any] =
    upsertPreferenceEndpoint.serverLogic[Requirements](token =>
      input => upsertPreferenceLogic.tupled(Tuple1(token) ++ input)
    )

  val getPreferenceEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    String,
    HttpError,
    Json,
    Any
  ] =
    secureEndpoint().get
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[NotFound](StatusCode.NotFound, jsonBody[NotFound].description("Preference not found"))
      )
      .in("preferences")
      .in(path[String]("key").example("main"))
      .out(jsonBody[Json])
      .description("Get preference.")

  val getPreferenceHttp: ZServerEndpoint[Requirements, Any] =
    getPreferenceEndpoint.serverLogic[Requirements](token => input => getPreferenceLogic(token, input))

  val deletePreferenceEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    String,
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint().delete
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[NotFound](StatusCode.NotFound, jsonBody[NotFound].description("Preference not found"))
      )
      .in("preferences")
      .in(path[String]("key").example("main"))
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description("Get preference.")

  val deletePreferenceHttp: ZServerEndpoint[Requirements, Any] =
    deletePreferenceEndpoint.serverLogic[Requirements](token => input => deletePreferenceLogic(token, input))

  val endpoints: List[AnyEndpoint] = List(
    upsertPreferenceEndpoint,
    getPreferenceEndpoint,
    deletePreferenceEndpoint
  ).map(_.endpoint.tag("user"))

  val http: List[ZServerEndpoint[Requirements, Any & ZioStreams]] = List(
    upsertPreferenceHttp,
    getPreferenceHttp,
    deletePreferenceHttp
  )
}
