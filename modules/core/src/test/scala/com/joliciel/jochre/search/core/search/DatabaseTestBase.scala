package com.joliciel.jochre.search.core.search

import com.joliciel.jochre.search.core.config.AppConfig
import com.joliciel.jochre.search.core.db.PostgresDatabase
import com.typesafe.config.ConfigFactory
import zio.{Runtime, ZIO}
import zio.config.typesafe.TypesafeConfigProvider

trait DatabaseTestBase {
  private val config = ConfigFactory.load().getConfig("jochre.search")

  private val configProviderLayer =
    Runtime.setConfigProvider(TypesafeConfigProvider.fromTypesafeConfig(config))

  private val configLayer = configProviderLayer >>> AppConfig.live
  private val transactorLayer = configLayer >>> PostgresDatabase.transactorLive

  val searchRepoLayer = transactorLayer >>> SearchRepo.live

  def getSearchRepo() =
    for {
      searchRepo <- ZIO.service[SearchRepo]
      _ <- searchRepo.deleteAll
    } yield searchRepo
}
