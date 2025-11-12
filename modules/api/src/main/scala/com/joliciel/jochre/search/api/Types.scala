package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.config.AppConfig
import com.joliciel.jochre.search.core.service.{PreferenceService, SearchService}
import doobie.Transactor
import zio.{RIO, Task}
import com.joliciel.jochre.search.core.service.StatsService
import com.joliciel.jochre.search.core.service.HttpRequestService

object Types {
  type Requirements = AppConfig & Transactor[Task] & SearchService & PreferenceService & StatsService &
    HttpRequestService

  type AppTask[T] = RIO[Requirements, T]
}
