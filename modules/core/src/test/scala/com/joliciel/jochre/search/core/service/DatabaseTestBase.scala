package com.joliciel.jochre.search.core.service

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

  val preferenceRepoLayer = transactorLayer >>> PreferenceRepo.live

  def getPreferenceRepo() =
    for {
      preferenceRepo <- ZIO.service[PreferenceRepo]
      _ <- preferenceRepo.deleteAll
    } yield preferenceRepo
}