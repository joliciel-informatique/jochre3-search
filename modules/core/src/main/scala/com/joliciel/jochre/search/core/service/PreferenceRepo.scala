package com.joliciel.jochre.search.core.service

import doobie.Transactor
import doobie.implicits._
import io.circe.Json
import zio.{Task, ZIO, ZLayer}
import zio.interop.catz._
import doobie.postgres.circe.json.implicits._

private[service] case class PreferenceRepo(transactor: Transactor[Task]) {
  def upsertPreference(username: String, key: String, preference: Json): Task[Int] =
    sql"""INSERT INTO preferences VALUES($username, $key, $preference)
         | ON CONFLICT (username, key)
         | DO UPDATE SET preference = $preference
       """.stripMargin.update.run
      .transact(transactor)

  def getPreference(username: String, key: String): Task[Option[Json]] =
    sql"""SELECT preference FROM preferences
         | WHERE username=$username AND key=$key
       """.stripMargin
      .query[Json]
      .option
      .transact(transactor)

  def deletePreference(username: String, key: String): Task[Int] =
    sql"""DELETE FROM preferences
          WHERE username=$username AND key=$key
       """.stripMargin.update.run
      .transact(transactor)

  private val deleteAllPreferences: Task[Int] =
    sql"""DELETE FROM preferences""".update.run.transact(transactor)

  private[service] val deleteAll: Task[Int] =
    for {
      count <- deleteAllPreferences
    } yield count
}

object PreferenceRepo {
  val live: ZLayer[Transactor[Task], Nothing, PreferenceRepo] =
    ZLayer {
      for {
        transactor <- ZIO.service[Transactor[Task]]
      } yield PreferenceRepo(transactor)
    }
}
