package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.DocReference
import sttp.tapir.{Codec, CodecFormat, DecodeResult, Schema}
import sttp.tapir.SchemaType.{SInteger, SString}

trait TapirSchemaSupport {
  implicit val codec_docId: Codec[String, DocReference, CodecFormat.TextPlain] =
    Codec.string.mapDecode(s => DecodeResult.Value(DocReference(s)))(_.ref)
  implicit val schema_docId: Schema[DocReference] = Schema(SString(), description = Some("DocId"))

  def schemaForLong[T]: Schema[T] = Schema(SInteger[T]()).format("int64")
}
