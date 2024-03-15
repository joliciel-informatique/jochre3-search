package com.joliciel.jochre.search.core.config

import zio.config._
import zio.config.magnolia._
import zio.{Config, ZIO, ZLayer}

case class AppConfig(dbConfig: DBConfig)

object AppConfig {
  private val dbConfig: Config[DBConfig] = deriveConfig[DBConfig].mapKey(toKebabCase).nested("database")

  val dbLayer: ZLayer[AppConfig, Nothing, DBConfig] = ZLayer.fromZIO(ZIO.service[AppConfig].map(_.dbConfig))

  val live: ZLayer[Any, Config.Error, AppConfig] = {
    ZLayer {
      for {
        db <- ZIO.config(dbConfig)
      } yield AppConfig(db)
    }
  }
}
