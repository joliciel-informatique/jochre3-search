package com.joliciel.jochre.search.core.service

import io.circe.Json
import io.circe.parser._
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZLayer}
import com.joliciel.jochre.search.core.TimeUnit
import com.joliciel.jochre.search.core.UsageStats
import java.time.Instant
import com.joliciel.jochre.search.core.TopUserStats

trait StatsService {
  def getUsageStats(timeUnit: TimeUnit, startDate: Instant, endDate: Instant): Task[UsageStats]

  def getTopUsers(startDate: Instant, endDate: Instant, maxBins: Int): Task[TopUserStats]
}

private[service] case class StatsServiceImpl(
    statsRepo: StatsRepo
) extends StatsService {
  private val log = LoggerFactory.getLogger(getClass)

  override def getUsageStats(timeUnit: TimeUnit, startDate: Instant, endDate: Instant): Task[UsageStats] =
    statsRepo.getUsageStats(timeUnit, startDate, endDate)

  override def getTopUsers(startDate: Instant, endDate: Instant, maxBins: Int): Task[TopUserStats] =
    statsRepo.getTopUsers(startDate, endDate, maxBins)
}

object StatsService {
  lazy val live: ZLayer[StatsRepo, Nothing, StatsService] =
    ZLayer.fromFunction(StatsServiceImpl(_))
}
