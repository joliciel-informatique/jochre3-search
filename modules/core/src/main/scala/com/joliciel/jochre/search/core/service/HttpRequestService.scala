package com.joliciel.jochre.search.core.service

import io.circe.Json
import io.circe.parser._
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZLayer}
import com.joliciel.jochre.search.core.TimeUnit
import com.joliciel.jochre.search.core.UsageStats
import java.time.Instant
import com.joliciel.jochre.search.core.TopUserStats
import com.joliciel.jochre.search.core.HttpRequestData

trait HttpRequestService {
  def insertHttpRequest(httpRequestData: HttpRequestData): Task[Int]
}

private[service] case class HttpRequestServiceImpl(
    httpRequestRepo: HttpRequestRepo
) extends HttpRequestService {
  private val log = LoggerFactory.getLogger(getClass)

  override def insertHttpRequest(httpRequestData: HttpRequestData): Task[Int] =
    httpRequestRepo.insertHttpRequest(httpRequestData)
}

object HttpRequestService {
  lazy val live: ZLayer[HttpRequestRepo, Nothing, HttpRequestService] =
    ZLayer.fromFunction(HttpRequestServiceImpl(_))
}
