package com.joliciel.jochre.search.core.service

import io.circe.Json
import io.circe.parser._
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZLayer}

trait PreferenceService {
  def upsertPreference(username: String, key: String, preference: Json): Task[Int]
  def getPreference(username: String, key: String): Task[Option[Json]]
  def deletePreference(username: String, key: String): Task[Int]
}

private[service] case class PreferenceServiceImpl(
    preferenceRepo: PreferenceRepo
) extends PreferenceService {
  private val log = LoggerFactory.getLogger(getClass)

  override def upsertPreference(username: String, key: String, preference: Json): Task[Int] =
    preferenceRepo.upsertPreference(username, key, preference)

  override def getPreference(username: String, key: String): Task[Option[Json]] =
    preferenceRepo.getPreference(username, key)

  override def deletePreference(username: String, key: String): Task[Int] =
    preferenceRepo.deletePreference(username, key)
}

object PreferenceService {
  lazy val live: ZLayer[PreferenceRepo, Nothing, PreferenceService] =
    ZLayer.fromFunction(PreferenceServiceImpl(_))
}
