package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.config.AppConfig
import com.joliciel.jochre.search.core.db.PostgresDatabase
import com.typesafe.config.ConfigFactory
import doobie.Transactor
import zio.{Config, Runtime, Scope, Task, ZIO, ZLayer}
import zio.config.typesafe.TypesafeConfigProvider

trait DatabaseTestBase {
  private val config = ConfigFactory.load().getConfig("jochre.search")

  private val configProviderLayer =
    Runtime.setConfigProvider(TypesafeConfigProvider.fromTypesafeConfig(config))

  private val configLayer: ZLayer[Any, Throwable, AppConfig] = configProviderLayer >>> AppConfig.live
  val transactorLayer: ZLayer[Scope, Throwable, Transactor[Task]] =
    configLayer >>> PostgresDatabase.transactorLive

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

  val suggestionRepoLayer = transactorLayer >>> SuggestionRepo.live

  def getSuggestionRepo() =
    for {
      suggestionRepo <- ZIO.service[SuggestionRepo]
      _ <- suggestionRepo.deleteAll
    } yield suggestionRepo

  val statsRepoLayer = transactorLayer >>> StatsRepo.live

  def getStatsRepo() =
    for {
      statsRepo <- ZIO.service[StatsRepo]
    } yield statsRepo
}
