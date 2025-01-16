package com.joliciel.jochre.search.core.lucene.tokenizer

import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.{TokenFilter, TokenStream}

import scala.annotation.tailrec

private[lucene] class SkipPunctuationFilter(input: TokenStream) extends TokenFilter(input) {
  private val charTerm = addAttribute(classOf[CharTermAttribute])
  private val punctuationRegex = raw"(?U)[\p{Punct}&&[^=+$$%€°£#<>]]+".r

  @tailrec
  final override def incrementToken: Boolean = {
    if (hasNextToken()) {
      val buffer = new String(charTerm.buffer()).subSequence(0, charTerm.length())
      if (punctuationRegex.matches(buffer)) {
        this.incrementToken()
      } else {
        true
      }
    } else {
      false
    }
  }

  private def hasNextToken(): Boolean = {
    input.incrementToken()
  }
}
