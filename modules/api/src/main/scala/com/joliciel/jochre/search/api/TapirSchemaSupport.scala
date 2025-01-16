package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.DocReference
import io.circe.Json
import io.circe.parser._
import sttp.tapir.SchemaType.{SInteger, SString}
import sttp.tapir.{Codec, CodecFormat, DecodeResult, Schema}

trait TapirSchemaSupport {
  given Codec[String, DocReference, CodecFormat.TextPlain] =
    Codec.string.mapDecode(s => DecodeResult.Value(DocReference(s)))(_.ref)
  given Schema[DocReference] = Schema(SString(), description = Some("DocReference"))

  given Codec[String, Json, CodecFormat.TextPlain] =
    Codec.string.mapDecode(s =>
      parse(s) match {
        case Left(failure) => DecodeResult.Error(s, failure)
        case Right(json)   => DecodeResult.Value(json)
      }
    )(_.spaces2)
  given Schema[Json] = Schema(SString(), description = Some("Json"))
}
