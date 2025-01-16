package com.joliciel.jochre.search.api.search

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

trait SearchSchemaSupport extends TapirSchemaSupport with SearchProtocol {
  given Codec[String, Highlight, CodecFormat.TextPlain] =
    Codec.string
      .mapDecode(s => decode[Highlight](s).fold(DecodeResult.Error(s, _), DecodeResult.Value(_)))(_.asJson.noSpaces)

  given Schema[DocRev] = Schema.schemaForLong.description("DocRev").map((rev: Long) => Some(DocRev(rev)))(_.rev)
  given Schema[DocMetadata] = Schema.derived
  given Schema[Highlight] = Schema.derived
  given given_schema_seq_highlight: Schema[Seq[Highlight]] = Schema.schemaForIterable[Highlight, Seq]
  given Schema[Snippet] = Schema.derived
  given given_schema_seq_snippet: Schema[Seq[Snippet]] = Schema.schemaForIterable[Snippet, Seq]
  given Schema[SearchResult] = Schema.derived
  given given_schema_seq_searchResult: Schema[Seq[SearchResult]] = Schema.schemaForIterable[SearchResult, Seq]
}
