package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.api.TapirSchemaSupport
import io.circe.parser._
import io.circe.syntax._
import sttp.tapir.SchemaType.SString
import sttp.tapir.{Codec, CodecFormat, DecodeResult, Schema}

trait SearchSchemaSupport extends TapirSchemaSupport with SearchProtocol {
  implicit val codec_docId: Codec[String, DocId, CodecFormat.TextPlain] =
    Codec.string.mapDecode(s => DecodeResult.Value(DocId(s)))(_.id)
  implicit val schema_docId: Schema[DocId] = Schema(SString(), description = Some("DocId"))

  implicit val codec_highlight: Codec[String, Highlight, CodecFormat.TextPlain] =
    Codec.string
      .mapDecode(s => decode[Highlight](s).fold(DecodeResult.Error(s, _), DecodeResult.Value(_)))(_.asJson.noSpaces)
}
