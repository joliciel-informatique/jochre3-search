package com.joliciel.jochre.search.core

import com.joliciel.jochre.search.core.service.DocRev
import io.circe.{Decoder, Encoder}

import scala.util.Try

trait CoreProtocol {
  given Decoder[DocReference] = Decoder.decodeString.emapTry(str => Try(DocReference(str)))
  given Encoder[DocReference] = Encoder.encodeString.contramap[DocReference](_.ref)

  given Decoder[DocRev] = Decoder.decodeLong.emapTry(long => Try(DocRev(long)))
  given Encoder[DocRev] = Encoder.encodeLong.contramap[DocRev](_.rev)
}
