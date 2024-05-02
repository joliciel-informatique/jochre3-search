package com.joliciel.jochre.search.api.utilities

import com.joliciel.jochre.search.api.HttpError.{BadRequest, NotFound}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.{AuthenticationProvider, TokenAuthentication, ValidToken}
import com.joliciel.jochre.search.api.{HttpError, OkResponse, Roles, TapirSchemaSupport}
import com.joliciel.jochre.search.core.CoreProtocol
import io.circe.Json
import io.circe.generic.auto._
import io.circe.literal._
import shapeless.syntax.std.tuple._
import sttp.capabilities.zio.ZioStreams
import sttp.model.StatusCode
import sttp.tapir.AnyEndpoint
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.ztapir._

import scala.concurrent.ExecutionContext

case class UtilityApp(override val authenticationProvider: AuthenticationProvider, executionContext: ExecutionContext)
    extends TokenAuthentication
    with UtilityLogic
    with TapirSchemaSupport
    with CoreProtocol {
  implicit val ec: ExecutionContext = executionContext

  val putLogLevelEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (String, String),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint(Roles.maintenance)
      .errorOutVariant[HttpError](
        oneOfVariant[NotFound](StatusCode.NotFound, jsonBody[NotFound].description("Unknown log level"))
      )
      .put
      .in("utilities")
      .in("log")
      .description("Set the log level for a given package prefix to a given level")
      .in(query[String]("prefix"))
      .in(query[String]("level"))
      .description("Valid levels are ALL, TRACE, DEBUG, INFO, WARN, ERROR, OFF")
      .out(jsonBody[OkResponse].example(OkResponse()))

  val putLogLevelHttp: ZServerEndpoint[Requirements, Any] =
    putLogLevelEndpoint.serverLogic[Requirements](_ => input => (putLogLevelLogic _).tupled(input))

  val resetLogLevelEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    Unit,
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint(Roles.maintenance)
      .errorOutVariant[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description("Not sure why this would happen")
        )
      )
      .put
      .in("utilities")
      .in("resetLog")
      .description("Reset the log to its base configuration")
      .out(jsonBody[OkResponse].example(OkResponse()))

  val resetLogLevelHttp: ZServerEndpoint[Requirements, Any] =
    resetLogLevelEndpoint.serverLogic[Requirements](_ => _ => resetLogLogic())

  val endpoints: List[AnyEndpoint] = List(
    putLogLevelEndpoint,
    resetLogLevelEndpoint
  ).map(_.endpoint.tag("utilities"))

  val http: List[ZServerEndpoint[Requirements, Any with ZioStreams]] = List(
    putLogLevelHttp,
    resetLogLevelHttp
  )
}
