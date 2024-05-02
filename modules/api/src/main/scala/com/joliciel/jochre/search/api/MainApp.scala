package com.joliciel.jochre.search.api

import cats.syntax.all._
import com.comcast.ip4s.{Host, Port}
import com.joliciel.jochre.search.api.Types.AppTask
import com.joliciel.jochre.search.api.authentication.{AuthenticationProvider, AuthenticationProviderConfig}
import com.joliciel.jochre.search.api.index.IndexApp
import com.joliciel.jochre.search.api.search.SearchApp
import com.joliciel.jochre.search.api.users.UserApp
import com.joliciel.jochre.search.api.utilities.UtilityApp
import com.joliciel.jochre.search.core.config.AppConfig
import com.joliciel.jochre.search.core.db.PostgresDatabase
import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.service.{
  PreferenceRepo,
  PreferenceService,
  SearchRepo,
  SearchService,
  SuggestionRepo
}
import com.joliciel.jochre.search.yiddish.lucene.tokenizer.YiddishFilters
import com.typesafe.config.ConfigFactory
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.Origin
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware.{CORS, Logger}
import org.slf4j.LoggerFactory
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import zio._
import zio.config.typesafe.TypesafeConfigProvider
import zio.interop.catz._

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

object MainApp extends ZIOAppDefault {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.search")

  override val bootstrap: ZLayer[ZIOAppArgs, Throwable, Any] =
    Runtime.setConfigProvider(TypesafeConfigProvider.fromTypesafeConfig(config))

  private val authenticationProvider: ZIO[Any, Throwable, AuthenticationProvider] =
    ZIO.config[AuthenticationProviderConfig](AuthenticationProviderConfig.config).map { config =>
      AuthenticationProvider(config)
    }

  private def runServer(authenticationProvider: AuthenticationProvider, executor: Executor): Task[Unit] = {
    val searchDirectives: SearchApp = SearchApp(authenticationProvider, executor.asExecutionContext)
    val searchRoutes: HttpRoutes[AppTask] = ZHttp4sServerInterpreter().from(searchDirectives.http).toRoutes
    val indexDirectives: IndexApp = IndexApp(authenticationProvider, executor.asExecutionContext)
    val indexRoutes: HttpRoutes[AppTask] = ZHttp4sServerInterpreter().from(indexDirectives.http).toRoutes
    val userDirectives: UserApp = UserApp(authenticationProvider, executor.asExecutionContext)
    val userRoutes: HttpRoutes[AppTask] = ZHttp4sServerInterpreter().from(userDirectives.http).toRoutes
    val utilityDirectives: UtilityApp = UtilityApp(authenticationProvider, executor.asExecutionContext)
    val utilityRoutes: HttpRoutes[AppTask] = ZHttp4sServerInterpreter().from(utilityDirectives.http).toRoutes

    val version = sys.env.get("JOCHRE3_SEARCH_VERSION").getOrElse("0.1.0-SNAPSHOT")
    val swaggerDirectives =
      SwaggerInterpreter().fromEndpoints[AppTask](
        searchDirectives.endpoints ++ indexDirectives.endpoints ++ userDirectives.endpoints ++ utilityDirectives.endpoints,
        "Jochre Search Server",
        version
      )
    val swaggerRoutes: HttpRoutes[AppTask] = ZHttp4sServerInterpreter().from(swaggerDirectives).toRoutes

    val routes = searchRoutes <+> indexRoutes <+> userRoutes <+> utilityRoutes <+> swaggerRoutes

    val httpApp = Router("/" -> routes).orNotFound

    val corsPolicy = CORS.policy
      .withAllowCredentials(false)
      .withMaxAge(DurationInt(1).day)

    val hosts = config.getStringList("allow-origin-hosts").asScala.toSet

    val corsPolicyWithHosts = if (hosts.isEmpty) {
      log.info("Allowing all origins")
      corsPolicy.withAllowOriginAll
    } else {
      log.info(f"Parsing origins: ${hosts.mkString(", ")}")
      corsPolicy.withAllowOriginHost(
        hosts.flatMap { host =>
          Origin.parse(host) match {
            case Left(parseFailure) => throw new Exception(f"Cannot parse $host as host: ${parseFailure.details}")
            case Right(Origin.HostList(hosts)) =>
              log.info(f"Allowing origins: $hosts")
              hosts.toList
            case Right(Origin.Null) =>
              log.info(f"Null origin")
              Seq()
          }
        }
      )
    }

    val loggerService = Logger.httpApp[AppTask](
      logHeaders = false,
      logBody = false,
      redactHeadersWhen = _ => false,
      logAction = Some((msg: String) => ZIO.succeed(log.info(msg)))
    )(httpApp)

    val corsService = corsPolicyWithHosts.apply(loggerService)

    // Starting the server
    val server = EmberServerBuilder
      .default[AppTask]
      .withHost(Host.fromString(config.getString("host")).get)
      .withPort(Port.fromInt(config.getInt("port")).get)
      .withHttpApp(corsService)
      .build
      .allocated
      .map(_._1) *> ZIO.never

    server
      .provide(
        Scope.default,
        AppConfig.live,
        PostgresDatabase.transactorLive,
        SearchRepo.live,
        SuggestionRepo.live,
        YiddishFilters.live,
        JochreIndex.live,
        SearchService.live,
        PreferenceRepo.live,
        PreferenceService.live
      )
  }

  private def app: Task[Unit] = {
    Runtime.setReportFatal { t =>
      t.printStackTrace()
      try {
        java.lang.System.exit(-1)
        throw t
      } catch { case _: Throwable => throw t }
    }
    for {
      authenticationProvider <- authenticationProvider
      executor <- ZIO.executor
      server <- runServer(authenticationProvider, executor)
    } yield server
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to build server", error))

  override def run: URIO[Any, ExitCode] =
    app.exitCode
}
