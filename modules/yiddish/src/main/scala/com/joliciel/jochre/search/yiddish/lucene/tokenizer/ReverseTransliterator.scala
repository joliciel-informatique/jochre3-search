package com.joliciel.jochre.search.yiddish.lucene.tokenizer

import com.joliciel.jochre.ocr.yiddish.YivoTransliterator
import com.joliciel.jochre.search.core.text.TextNormalizer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.{TokenFilter, TokenStream}

import java.util.Locale

private[yiddish] class ReverseTransliterator(input: TokenStream) extends TokenFilter(input) {
  private val termAttr = addAttribute(classOf[CharTermAttribute])

  private val textNormalizer = TextNormalizer(locale = Locale.forLanguageTag("yi"))

  private val latinRegex = """(?U)\p{IsLatin}+""".r
  final override def incrementToken: Boolean = {
    if (input.incrementToken()) {
      val term = termAttr.toString
      if (latinRegex.matches(term)) {
        val alternative = YivoTransliterator.detransliterate(term)
        val combined = YivoTransliterator.replaceWithDecomposed(alternative)
        val normalized = textNormalizer.normalize(combined)
        termAttr.copyBuffer(normalized.toCharArray, 0, normalized.size)
      }
      true
    } else {
      false
    }
  }
}
