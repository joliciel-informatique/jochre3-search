package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.config.AppConfig
import com.joliciel.jochre.search.core.service.{PreferenceService, SearchService}
import doobie.Transactor
import zio.{RIO, Task}

object Types {
  type Requirements = AppConfig with Transactor[Task] with SearchService with PreferenceService

  type AppTask[T] = RIO[Requirements, T]
}
