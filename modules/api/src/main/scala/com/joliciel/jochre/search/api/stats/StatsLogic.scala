package com.joliciel.jochre.search.api.stats

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper, OkResponse, UnparseableDateException}
import com.joliciel.jochre.search.core.{PreferenceNotFound, TimeUnit, UsageStats, StatsHelper}
import com.joliciel.jochre.search.core.service.PreferenceService
import io.circe.Json
import zio.ZIO
import com.joliciel.jochre.search.core.service.StatsService
import java.time.format.DateTimeFormatter
import java.time.Instant
import java.time.ZoneOffset
import java.time.LocalDateTime
import java.time.format.DateTimeParseException
import java.time.LocalDate
import java.time.ZoneId
import com.joliciel.jochre.search.core.TopUserStats

trait StatsLogic extends HttpErrorMapper {
  def getUsageStatsLogic(
      token: ValidToken,
      timeUnit: TimeUnit,
      startDate: String,
      endDate: String
  ): ZIO[Requirements, HttpError, UsageStats] = {
    for {
      statsService <- ZIO.service[StatsService]
      startDateAsInstant <- ZIO.attempt(stringToInstant(startDate))
      endDateAsInstant <- ZIO.attempt(stringToInstant(endDate))
      usageStats <- statsService.getUsageStats(timeUnit, startDateAsInstant, endDateAsInstant)
    } yield usageStats
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to get usage stats", error))
    .mapError(mapToHttpError)

  def getTopUserStatsLogic(
      token: ValidToken,
      startDate: String,
      endDate: String,
      maxBins: Int
  ): ZIO[Requirements, HttpError, TopUserStats] = {
    for {
      statsService <- ZIO.service[StatsService]
      startDateAsInstant <- ZIO.attempt(stringToInstant(startDate))
      endDateAsInstant <- ZIO.attempt(stringToInstant(endDate))
      topUserStats <- statsService.getTopUsers(startDateAsInstant, endDateAsInstant, maxBins)
    } yield topUserStats
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to get top user stats", error))
    .mapError(mapToHttpError)

  private val dayFormatter = DateTimeFormatter
    .ofPattern("yyyy-MM-dd")

  private def stringToInstant(dateString: String): Instant = try {
    val ldt = LocalDate.parse(dateString, dayFormatter)
    ldt.atStartOfDay(ZoneId.systemDefault()).toInstant()
  } catch {
    case ex: DateTimeParseException => throw new UnparseableDateException(dateString)
  }
}
