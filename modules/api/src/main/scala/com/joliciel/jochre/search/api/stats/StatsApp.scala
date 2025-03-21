package com.joliciel.jochre.search.api.stats

import com.joliciel.jochre.search.api.HttpError.{BadRequest, NotFound}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.{AuthenticationProvider, TokenAuthentication, ValidToken}
import com.joliciel.jochre.search.api.{HttpError, OkResponse, TapirSchemaSupport}
import com.joliciel.jochre.search.core.{CoreProtocol, TimeUnit, UsageStats, StatsHelper, TopUserStats}
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
import com.joliciel.jochre.search.api.Roles

case class StatsApp(override val authenticationProvider: AuthenticationProvider, executionContext: ExecutionContext)
    extends TokenAuthentication
    with StatsLogic
    with StatsSchemaSupport
    with CoreProtocol {
  given ExecutionContext = executionContext

  val getUsageStatsEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (TimeUnit, String, String),
    HttpError,
    UsageStats,
    Any
  ] =
    secureEndpoint(Roles.stats).get
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description("Start date or end date in bad format")
        )
      )
      .in("stats")
      .in("usage")
      .in(query[TimeUnit]("time-unit").example(TimeUnit.Month))
      .in(query[String]("start-date").example("2025-01-01").description("Start date (inclusive)"))
      .in(query[String]("end-date").example("2025-12-31").description("End date (inclusive)"))
      .out(jsonBody[UsageStats].example(StatsHelper.usageStatsExample))
      .description("Get usage statistics for a given period by time unit.")

  val getUsageStatsHttp: ZServerEndpoint[Requirements, Any] =
    getUsageStatsEndpoint.serverLogic[Requirements](token => input => getUsageStatsLogic.tupled(Tuple1(token) ++ input))

  val getTopUserStatsEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (String, String, Int),
    HttpError,
    TopUserStats,
    Any
  ] =
    secureEndpoint(Roles.stats).get
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description("Start date or end date in bad format")
        )
      )
      .in("stats")
      .in("top-users")
      .in(query[String]("start-date").example("2025-01-01").description("Start date (inclusive)"))
      .in(query[String]("end-date").example("2025-12-31").description("End date (inclusive)"))
      .in(query[Int]("max-bins").example(42).description("The max user bins to return."))
      .out(jsonBody[TopUserStats].example(StatsHelper.topUserStatsExample))
      .description("Get top user statistics for a given period.")

  val getTopUserStatsHttp: ZServerEndpoint[Requirements, Any] =
    getTopUserStatsEndpoint.serverLogic[Requirements](token =>
      input => getTopUserStatsLogic.tupled(Tuple1(token) ++ input)
    )

  val endpoints: List[AnyEndpoint] = List(
    getUsageStatsEndpoint,
    getTopUserStatsEndpoint
  ).map(_.endpoint.tag("stats"))

  val http: List[ZServerEndpoint[Requirements, Any & ZioStreams]] = List(
    getUsageStatsHttp,
    getTopUserStatsHttp
  )
}
