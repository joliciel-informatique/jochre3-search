package com.joliciel.jochre.search.api.stats

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper, OkResponse}
import com.joliciel.jochre.search.core.PreferenceNotFound
import com.joliciel.jochre.search.core.service.PreferenceService
import io.circe.Json
import zio.ZIO

trait StatsLogic extends HttpErrorMapper {
  def getUsageStatsLogic(
      token: ValidToken,
      timeUnit: TimeUnit,
      startDate: String,
      endDate: String
  ): ZIO[Requirements, HttpError, UsageStats] = {
    for {
      _ <- ZIO.service[PreferenceService]
    } yield StatsHelper.usageStatsExample
  }
}
