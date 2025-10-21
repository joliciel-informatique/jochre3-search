package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.text.Dehyphenator
import zio.ZLayer

trait WithDehyphenatorLayer {
  private val dehyphenator = new Dehyphenator {
    def dehyphenate(text: String): String = text
  }
  val dehyphenatorLayer = ZLayer.succeed(dehyphenator)
}
