package com.joliciel.jochre.search.yiddish

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.Word
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YiddishFiltersTest extends AnyFlatSpec with Matchers {
  "breakWord" should "keep Yiddish abbreviations intact" in {
    val content = "\"רמב\"ם\""
    val word = Word(content, Rectangle(0, 0, 70, 10), glyphs = Seq.empty, alternatives = Seq.empty, confidence = 1.0)
    val words = YiddishFilters.breakWord(word)
    val wordContents = Seq("\"", "רמב\"ם", "\"")
    words shouldEqual Seq(
      Word(
        wordContents(0),
        Rectangle(60, 0, 10, 10),
        glyphs = Seq.empty,
        alternatives = YiddishFilters.getAlternatives(wordContents(0)),
        confidence = 1.0
      ),
      Word(
        wordContents(1),
        Rectangle(10, 0, 50, 10),
        glyphs = Seq.empty,
        alternatives = YiddishFilters.getAlternatives(wordContents(1)),
        confidence = 1.0
      ),
      Word(
        wordContents(2),
        Rectangle(0, 0, 10, 10),
        glyphs = Seq.empty,
        alternatives = YiddishFilters.getAlternatives(wordContents(2)),
        confidence = 1.0
      )
    )
  }

}
