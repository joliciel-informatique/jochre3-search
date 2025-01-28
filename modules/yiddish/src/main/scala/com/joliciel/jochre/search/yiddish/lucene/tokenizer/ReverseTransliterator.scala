package com.joliciel.jochre.search.yiddish.lucene.tokenizer

import com.joliciel.jochre.ocr.yiddish.YivoTransliterator
import com.joliciel.jochre.search.core.text.TextNormalizer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.{TokenFilter, TokenStream}

import java.util.Locale
import org.apache.lucene.analysis.tokenattributes.PositionIncrementAttribute
import org.apache.lucene.util.AttributeSource

private[yiddish] class ReverseTransliterator(input: TokenStream) extends TokenFilter(input) {
  private val termAttr = addAttribute(classOf[CharTermAttribute])
  private val posIncAttr = addAttribute(classOf[PositionIncrementAttribute])

  private val textNormalizer = TextNormalizer(locale = Locale.forLanguageTag("yi"))

  private val latinRegex = """(?U)\p{IsLatin}+""".r

  private var originalText: Option[String] = None
  private var attributeState: AttributeSource.State = scala.compiletime.uninitialized

  final override def incrementToken: Boolean = {
    if (originalText.isDefined) {
      clearAttributes()
      restoreState(attributeState)
      originalText.foreach(t => termAttr.copyBuffer(t.toCharArray, 0, t.size))
      posIncAttr.setPositionIncrement(0)
      originalText = None
      true
    } else if (input.incrementToken()) {
      val term = termAttr.toString
      if (latinRegex.matches(term)) {
        val alternative = YivoTransliterator.detransliterate(term)
        val decomposed = YivoTransliterator.replaceWithDecomposed(alternative)
        val normalized = textNormalizer.normalize(decomposed)
        termAttr.copyBuffer(normalized.toCharArray, 0, normalized.size)
        originalText = Some(term)
        attributeState = captureState()
      }
      true
    } else {
      false
    }
  }
}
