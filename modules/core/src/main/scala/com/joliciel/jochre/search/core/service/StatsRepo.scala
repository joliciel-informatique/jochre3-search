package com.joliciel.jochre.search.core.service

import doobie.Transactor
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.postgres.implicits._
import doobie.util.update.Update
import zio.interop.catz._
import zio.{Task, ZIO, ZLayer}
import doobie.util.meta.Meta
import com.joliciel.jochre.search.core.{TimeUnit, UsageStats}
import java.time.Instant
import com.joliciel.jochre.search.core.UsageStatsBin
import java.time.format.DateTimeFormatter
import java.time.ZoneOffset
import doobie.util.fragment.Fragment
import org.slf4j.LoggerFactory
import java.time.ZoneId

private[service] case class StatsRepo(transactor: Transactor[Task]) extends DoobieSupport {
  private val log = LoggerFactory.getLogger(getClass)

  private val dayFormatter = DateTimeFormatter
    .ofPattern("yyyy-MM-dd")
    .withZone(ZoneId.systemDefault())

  private val monthFormatter = DateTimeFormatter
    .ofPattern("yyyy-MM")
    .withZone(ZoneId.systemDefault())

  private val yearFormatter = DateTimeFormatter
    .ofPattern("yyyy")
    .withZone(ZoneId.systemDefault())

  private def toLabel(timeUnit: TimeUnit, instant: Instant): String = timeUnit match {
    case TimeUnit.Day   => dayFormatter.format(instant)
    case TimeUnit.Month => monthFormatter.format(instant)
    case TimeUnit.Year  => yearFormatter.format(instant)
  }

  def getUsageStats(timeUnit: TimeUnit, startDate: Instant, endDate: Instant): Task[UsageStats] = {
    for {
      userStats <- getUserStats(timeUnit, startDate, endDate)
      queryStats <- getQueryStats(timeUnit, startDate, endDate)
    } yield {
      val instantKeys = userStats.keySet ++ queryStats.keySet
      val bins = instantKeys
        .map { instant =>
          val userCount = userStats.getOrElse(instant, 0)
          val queryCount = queryStats.getOrElse(instant, 0)
          log.info(f"At ${instant.toString()} found $userCount users and $queryCount queries")
          (instant, userCount, queryCount)
        }
        .toSeq
        .sortBy(_._1)
        .map { case (instant, userCount, queryCount) =>
          UsageStatsBin(toLabel(timeUnit, instant), userCount, queryCount)
        }
      UsageStats(bins)
    }
  }

  private def getUserStats(timeUnit: TimeUnit, startDate: Instant, endDate: Instant): Task[Map[Instant, Int]] = {
    val sqlTimeUnit = timeUnit match {
      case TimeUnit.Day   => "day"
      case TimeUnit.Month => "month"
      case TimeUnit.Year  => "year"
    }
    (fr"SELECT date_trunc(" ++ Fragment.const(
      f"'$sqlTimeUnit'"
    ) ++ fr""", executed) as time_unit, COUNT(distinct username) AS users
      | FROM query
      | WHERE executed >= $startDate
      | AND executed < $endDate
      | GROUP BY time_unit
      | ORDER BY time_unit ASC
    """.stripMargin)
      .query[(Instant, Int)]
      .to[Seq]
      .transact(transactor)
      .map(_.toMap)
  }

  private def getQueryStats(timeUnit: TimeUnit, startDate: Instant, endDate: Instant): Task[Map[Instant, Int]] = {
    val sqlTimeUnit = timeUnit match {
      case TimeUnit.Day   => "day"
      case TimeUnit.Month => "month"
      case TimeUnit.Year  => "year"
    }
    (fr"SELECT date_trunc(" ++ Fragment.const(
      f"'$sqlTimeUnit'"
    ) ++ fr""", executed) as time_unit, COUNT(distinct id) AS queries
      | FROM query
      | WHERE executed >= $startDate
      | AND executed < $endDate
      | GROUP BY time_unit
      | ORDER BY time_unit ASC
    """.stripMargin)
      .query[(Instant, Int)]
      .to[Seq]
      .transact(transactor)
      .map(_.toMap)
  }
}

object StatsRepo {
  val live: ZLayer[Transactor[Task], Nothing, StatsRepo] =
    ZLayer {
      for {
        transactor <- ZIO.service[Transactor[Task]]
      } yield StatsRepo(transactor)
    }
}
