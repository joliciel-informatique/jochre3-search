package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.DocReference
import io.circe.Json
import io.circe.parser._
import sttp.tapir.{Codec, CodecFormat, DecodeResult, Schema}
import sttp.tapir.SchemaType.{SInteger, SString}

trait TapirSchemaSupport {
  implicit val codec_docId: Codec[String, DocReference, CodecFormat.TextPlain] =
    Codec.string.mapDecode(s => DecodeResult.Value(DocReference(s)))(_.ref)
  implicit val schema_docId: Schema[DocReference] = Schema(SString(), description = Some("DocId"))

  def schemaForLong[T]: Schema[T] = Schema(SInteger[T]()).format("int64")

  implicit val codec_json: Codec[String, Json, CodecFormat.TextPlain] =
    Codec.string.mapDecode(s =>
      parse(s) match {
        case Left(failure) => DecodeResult.Error(s, failure)
        case Right(json)   => DecodeResult.Value(json)
      }
    )(_.spaces2)
  implicit val schema_json: Schema[Json] = Schema(SString(), description = Some("Json"))
}
