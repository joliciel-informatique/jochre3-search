package com.joliciel.jochre.search.core

import zio._

package object config {
  final case class DBConfig(
    className: String,
    url: String,
    username: String,
    password: String,
    connectionTimeout: Int,
    minimumIdle: Int,
    maximumPoolSize: Int
  )
}
