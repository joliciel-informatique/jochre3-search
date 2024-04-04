package com.joliciel.jochre.search.core

import com.joliciel.jochre.search.core.service.DocRev
import io.circe.{Decoder, Encoder}

import scala.util.Try

trait CoreProtocol {
  implicit val jsonDecoder_docRef: Decoder[DocReference] = Decoder.decodeString.emapTry(str => Try(DocReference(str)))
  implicit val jsonEncoder_docRef: Encoder[DocReference] = Encoder.encodeString.contramap[DocReference](_.ref)

  implicit val jsonDecoder_docRev: Decoder[DocRev] = Decoder.decodeLong.emapTry(long => Try(DocRev(long)))
  implicit val jsonEncoder_docRev: Encoder[DocRev] = Encoder.encodeLong.contramap[DocRev](_.rev)
}
