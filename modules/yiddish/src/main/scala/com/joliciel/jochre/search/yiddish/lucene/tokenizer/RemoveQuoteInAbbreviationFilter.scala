package com.joliciel.jochre.search.yiddish.lucene.tokenizer

import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.{TokenFilter, TokenStream}

private[lucene] class RemoveQuoteInAbbreviationFilter(input: TokenStream) extends TokenFilter(input) {
  private val termAttr = addAttribute(classOf[CharTermAttribute])

  private val abbreviationRegex = raw"""(?U)\p{L}+([‛’“'"’״׳]\p{L}+)+""".r

  final override def incrementToken: Boolean = {
    if (input.incrementToken()) {
      val term = termAttr.toString
      if (abbreviationRegex.matches(term)) {
        val withoutInternalQuotes = term.replaceAll(raw"""[‛’“'"’״׳]""", "")
        termAttr.copyBuffer(withoutInternalQuotes.toCharArray, 0, withoutInternalQuotes.size)
      }
      true
    } else {
      false
    }
  }
}