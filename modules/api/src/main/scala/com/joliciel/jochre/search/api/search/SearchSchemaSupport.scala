package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.api.TapirSchemaSupport
import io.circe.parser._
import io.circe.syntax._
import sttp.tapir.{Codec, CodecFormat, DecodeResult}

trait SearchSchemaSupport extends TapirSchemaSupport with SearchProtocol {
  implicit val codec_highlight: Codec[String, Highlight, CodecFormat.TextPlain] =
    Codec.string
      .mapDecode(s => decode[Highlight](s).fold(DecodeResult.Error(s, _), DecodeResult.Value(_)))(_.asJson.noSpaces)
}
