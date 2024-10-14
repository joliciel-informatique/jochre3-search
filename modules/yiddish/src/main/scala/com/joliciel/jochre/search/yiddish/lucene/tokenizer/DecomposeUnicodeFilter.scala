package com.joliciel.jochre.search.yiddish.lucene.tokenizer

import com.joliciel.jochre.ocr.yiddish.YivoTransliterator
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.{TokenFilter, TokenStream}

private[yiddish] class DecomposeUnicodeFilter(input: TokenStream) extends TokenFilter(input) {
  private val termAttr = addAttribute(classOf[CharTermAttribute])

  final override def incrementToken: Boolean = {
    if (input.incrementToken()) {
      val term = termAttr.toString
      val decomposed = YivoTransliterator.replaceWithDecomposed(term)
      if (decomposed != term) {
        termAttr.copyBuffer(decomposed.toCharArray, 0, decomposed.size)
      }
      true
    } else {
      false
    }
  }
}
