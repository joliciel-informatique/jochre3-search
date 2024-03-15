package com.joliciel.jochre.search.api

import sttp.tapir.Schema
import sttp.tapir.SchemaType.SInteger

trait TapirSchemaSupport {
  def schemaForLong[T]: Schema[T] = Schema(SInteger[T]()).format("int64")
}
