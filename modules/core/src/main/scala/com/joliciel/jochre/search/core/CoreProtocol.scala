package com.joliciel.jochre.search.core

import io.circe.{Decoder, Encoder}

import scala.util.Try

trait CoreProtocol {
  implicit val jsonDecoder_docId: Decoder[DocReference] = Decoder.decodeString.emapTry(str => Try(DocReference(str)))
  implicit val jsonEncoder_docId: Encoder[DocReference] = Encoder.encodeString.contramap[DocReference](_.ref)
}
