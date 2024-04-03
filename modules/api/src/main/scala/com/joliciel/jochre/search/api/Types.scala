package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.config.AppConfig
import com.joliciel.jochre.search.core.service.SearchService
import doobie.Transactor
import zio.{RIO, Task}

object Types {
  type Requirements = AppConfig with Transactor[Task] with SearchService

  type AppTask[T] = RIO[Requirements, T]
}
