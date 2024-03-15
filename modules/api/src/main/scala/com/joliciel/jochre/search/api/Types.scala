package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.config.AppConfig
import doobie.Transactor
import zio.{RIO, Task}

object Types {
  type Requirements = AppConfig with Transactor[Task]

  type AppTask[T] = RIO[Requirements, T]
}
