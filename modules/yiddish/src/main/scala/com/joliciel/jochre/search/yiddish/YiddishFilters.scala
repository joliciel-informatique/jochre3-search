package com.joliciel.jochre.search.yiddish

import com.joliciel.jochre.ocr.core.model.SpellingAlternative
import com.joliciel.jochre.ocr.yiddish.lexicon.YivoLexicon
import com.joliciel.jochre.ocr.yiddish.{YiddishAltoTransformer, YiddishConfig}
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import com.joliciel.jochre.search.yiddish.lucene.tokenizer.{
  DecomposeUnicodeFilter,
  RemoveQuoteInAbbreviationFilter,
  ReverseTransliterator
}
import org.apache.lucene.analysis.TokenStream
import zio.ZLayer

object YiddishFilters extends LanguageSpecificFilters {
  private val yiddishConfig = YiddishConfig.fromConfig
  private val yivoLexicon = YivoLexicon.fromYiddishConfig(yiddishConfig)

  override val postTokenizationFilterForSearch: Option[TokenStream => TokenStream] = Some { input: TokenStream =>
    val reverseTransliterator = new ReverseTransliterator(input)
    val decomposeUnicodeFilter = new DecomposeUnicodeFilter(reverseTransliterator)
    val removeQuoteInAbbreviationFilter = new RemoveQuoteInAbbreviationFilter(decomposeUnicodeFilter)
    removeQuoteInAbbreviationFilter
  }

  override val postTokenizationFilterForIndex: Option[TokenStream => TokenStream] = Some { input: TokenStream =>
    new RemoveQuoteInAbbreviationFilter(input)
  }

  def getAlternatives(word: String): Seq[SpellingAlternative] = {
    val yivo = yivoLexicon.toYivo(word, false)
    if (yivo != word) {
      Seq(SpellingAlternative(YiddishAltoTransformer.Purpose.YIVO.entryName, yivo))
    } else {
      Seq.empty
    }
  }

  val live: ZLayer[Any, Throwable, LanguageSpecificFilters] = ZLayer.succeed(YiddishFilters)
}
