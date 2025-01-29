package com.joliciel.jochre.search.api.stats

import com.joliciel.jochre.search.api.TapirSchemaSupport
import com.joliciel.jochre.search.core.service.{DocRev, Highlight, SearchProtocol, Snippet}
import io.circe.parser._
import io.circe.syntax._
import sttp.tapir.{Codec, CodecFormat, DecodeResult}
import sttp.tapir.generic.auto._
import sttp.tapir.Schema
import sttp.tapir.Schema.schemaForIterable
import com.joliciel.jochre.search.core.service.SearchResult
import com.joliciel.jochre.search.core.DocMetadata

trait StatsSchemaSupport extends TapirSchemaSupport with SearchProtocol {
  given Codec[String, TimeUnit, CodecFormat.TextPlain] = Codec.derivedEnumeration[String, TimeUnit].defaultStringBased
}
