package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.CoreProtocol
import io.circe.{Decoder, Encoder}

import scala.util.Try

trait SearchProtocol extends CoreProtocol {
  given Decoder[Highlight] = Decoder.decodeList[Int].emapTry(ints => Try(Highlight(ints(0), ints(1))))
  given Encoder[Highlight] = Encoder.encodeList[Int].contramap[Highlight](h => List(h.start, h.end))
}
