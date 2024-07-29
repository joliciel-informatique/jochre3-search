package com.joliciel.jochre.search.core.lucene.tokenizer

import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, PositionIncrementAttribute}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}

private[lucene] class IgnorePunctuationFilter(input: TokenStream) extends TokenFilter(input) {
  private val charTerm = addAttribute(classOf[CharTermAttribute])
  private val punctuationRegex = raw"(?U)[\p{Punct}&&[^=+$$%€°£#<>]]+".r

  private val posIncAttr = addAttribute(classOf[PositionIncrementAttribute])

  // We cannot set the initial position increment to zero (if the field starts with punctuation)
  private var punctuationPositionIncrement = 1

  final override def incrementToken: Boolean = {
    if (input.incrementToken()) {
      val buffer = new String(charTerm.buffer()).subSequence(0, charTerm.length())
      if (punctuationRegex.matches(buffer)) {
        posIncAttr.setPositionIncrement(punctuationPositionIncrement)
      }
      if (punctuationPositionIncrement > 0) {
        punctuationPositionIncrement = 0
      }
      true
    } else {
      // Set back to 1 for next field
      punctuationPositionIncrement = 1
      false
    }
  }
}
