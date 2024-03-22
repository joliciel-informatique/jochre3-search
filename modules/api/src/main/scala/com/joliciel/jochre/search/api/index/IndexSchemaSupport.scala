package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.api.TapirSchemaSupport
import com.joliciel.jochre.search.core.DocReference
import io.circe.parser._
import io.circe.syntax._
import sttp.tapir.SchemaType.SString
import sttp.tapir.{Codec, CodecFormat, DecodeResult, Schema}

trait IndexSchemaSupport extends TapirSchemaSupport {
  implicit val codec_docReference: Codec[String, DocReference, CodecFormat.TextPlain] =
    Codec.string.mapDecode(s => DecodeResult.Value(DocReference(s)))(_.ref)
  implicit val schema_docReference: Schema[DocReference] = Schema(SString(), description = Some("DocReference"))
}
