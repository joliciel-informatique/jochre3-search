package com.joliciel.jochre.search.core.lucene.tokenizer

import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, PositionIncrementAttribute}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}

private[lucene] class IgnorePunctuationFilter(input: TokenStream) extends TokenFilter(input) {
  private val charTerm = addAttribute(classOf[CharTermAttribute])
  private val punctuationRegex = raw"(?U)[\p{Punct}&&[^=+$$%€°£#<>]]+".r

  private val posIncAttr = addAttribute(classOf[PositionIncrementAttribute])

  final override def incrementToken: Boolean = {
    if (input.incrementToken()) {
      val buffer = new String(charTerm.buffer()).subSequence(0, charTerm.length())
      if (punctuationRegex.matches(buffer)) {
        posIncAttr.setPositionIncrement(0)
      }
      true
    } else {
      false
    }
  }
}
