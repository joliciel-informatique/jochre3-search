package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.search.core.text.TextNormalizer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.{TokenFilter, TokenStream}

class TextNormalizingFilter(input: TokenStream, textNormalizer: TextNormalizer) extends TokenFilter(input) {
  private val termAttr = addAttribute(classOf[CharTermAttribute])
  override def incrementToken(): Boolean = {
    if (input.incrementToken) {
      val term = termAttr.toString
      val normalized = textNormalizer.normalize(term)
      termAttr.copyBuffer(normalized.toCharArray, 0, normalized.length)
      true
    } else {
      false
    }
  }
}
