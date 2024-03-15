package com.joliciel.jochre.search.api.search

import io.circe.{Decoder, Encoder}

import scala.util.Try

trait SearchProtocol {
  implicit val jsonDecoder_docId: Decoder[DocId] = Decoder.decodeString.emapTry(str => Try(DocId(str)))
  implicit val jsonEncoder_docId: Encoder[DocId] = Encoder.encodeString.contramap[DocId](_.id)

  implicit val jsonDecoder_highlight: Decoder[Highlight] =
    Decoder.decodeList[Int].emapTry(ints => Try(Highlight(ints(0), ints(1))))
  implicit val jsonEncoder_highlight: Encoder[Highlight] =
    Encoder.encodeList[Int].contramap[Highlight](h => List(h.start, h.end))
}
