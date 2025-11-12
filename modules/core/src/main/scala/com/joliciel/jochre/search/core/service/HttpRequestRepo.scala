package com.joliciel.jochre.search.core.service

import cats.implicits._
import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Page, Word}
import com.joliciel.jochre.search.core.{DocReference, SearchCriterion, Sort}
import doobie._
import doobie.implicits._
import doobie.postgres.circe.jsonb.implicits._
import doobie.postgres.implicits._
import doobie.util.meta.Meta
import io.circe.generic.semiauto._
import zio._
import zio.interop.catz._

import java.time.Instant
import io.circe.Decoder
import io.circe.Encoder
import com.joliciel.jochre.search.core.HttpRequestData

private[service] case class HttpRequestRepo(transactor: Transactor[Task]) extends DoobieSupport {
  def insertHttpRequest(
      httpRequestData: HttpRequestData
  ): Task[Int] =
    sql"""INSERT INTO http_request (username, ip, method_and_url, querystring)
         | VALUES (${httpRequestData.username}, ${httpRequestData.ip}, ${httpRequestData.methodAndUrl}, ${httpRequestData.queryString})
         | """.stripMargin.update.run.transact(transactor)
}

object HttpRequestRepo {
  val live: ZLayer[Transactor[Task], Nothing, HttpRequestRepo] =
    ZLayer {
      for {
        transactor <- ZIO.service[Transactor[Task]]
      } yield HttpRequestRepo(transactor)
    }
}
