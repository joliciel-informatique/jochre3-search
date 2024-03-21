package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.core.CoreProtocol
import io.circe.{Decoder, Encoder}

import scala.util.Try

trait SearchProtocol extends CoreProtocol {
  implicit val jsonDecoder_highlight: Decoder[Highlight] =
    Decoder.decodeList[Int].emapTry(ints => Try(Highlight(ints(0), ints(1))))
  implicit val jsonEncoder_highlight: Encoder[Highlight] =
    Encoder.encodeList[Int].contramap[Highlight](h => List(h.start, h.end))
}
