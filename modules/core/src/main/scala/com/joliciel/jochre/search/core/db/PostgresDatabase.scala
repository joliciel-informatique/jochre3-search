package com.joliciel.jochre.search.core.db

import com.joliciel.jochre.search.core.config.{AppConfig, DBConfig}
import com.zaxxer.hikari.HikariConfig
import doobie.hikari.HikariTransactor
import doobie.{LogHandler, Transactor}
import org.flywaydb.core.Flyway
import org.slf4j.LoggerFactory
import zio._
import zio.interop.catz._
import zio.interop.catz.implicits._

import scala.concurrent.ExecutionContext

object PostgresDatabase {
  val log = LoggerFactory.getLogger(getClass)
  val logHandler: LogHandler[Task] = DoobieLogHandler.logHandler(log)
  
  private def migrate(config: DBConfig): Task[Unit] =
    ZIO.attempt {
      Flyway
        .configure()
        .dataSource(config.url, config.username, config.password)
        .load()
        .migrate()
    }.unit

  private def makeTransactor(
    config: DBConfig,
    ec: ExecutionContext
  ): RIO[Scope, Transactor[Task]] = {

    val hikariConfig = new HikariConfig()
    hikariConfig.setJdbcUrl(config.url)
    hikariConfig.setDriverClassName(config.className)
    hikariConfig.setUsername(config.username)
    hikariConfig.setPassword(config.password)
    hikariConfig.setConnectionTimeout(config.connectionTimeout)
    hikariConfig.setMinimumIdle(config.minimumIdle)
    hikariConfig.setMaximumPoolSize(config.maximumPoolSize)

    HikariTransactor
      .fromHikariConfigCustomEc[Task](hikariConfig, ec, Some(logHandler))
      .toScopedZIO
  }

  val transactorLive: URLayer[Scope & AppConfig, Transactor[Task]] =
    ZLayer
      .fromZIO(
        for {
          _ <- ZIO.logDebug("Constructing layer PostgresDatabase")
          appConfig <- ZIO.service[AppConfig]
          config = appConfig.dbConfig
          _ <- migrate(config)
          executionContext <- ZIO.descriptor.map(_.executor.asExecutionContext)
          transactor <- makeTransactor(config, executionContext)
        } yield transactor
        ).tapError(throwable => ZIO.logError(throwable.getMessage))
    .orDie
}
