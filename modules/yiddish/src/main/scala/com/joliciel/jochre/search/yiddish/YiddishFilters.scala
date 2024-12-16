package com.joliciel.jochre.search.yiddish

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{SpellingAlternative, Word}
import com.joliciel.jochre.ocr.yiddish.lexicon.YivoLexicon
import com.joliciel.jochre.ocr.yiddish.{YiddishAltoTransformer, YiddishConfig, YiddishTextSimpifier}
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import com.joliciel.jochre.search.yiddish.lucene.tokenizer.{
  DecomposeUnicodeFilter,
  RemoveQuoteInAbbreviationFilter,
  ReverseTransliterator
}
import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.TokenStream
import org.slf4j.LoggerFactory
import zio.ZLayer

import scala.jdk.CollectionConverters._

object YiddishFilters extends LanguageSpecificFilters {
  private val log = LoggerFactory.getLogger(getClass)
  private val yiddishConfig = YiddishConfig.fromConfig
  private val yivoLexicon = YivoLexicon.fromYiddishConfig(yiddishConfig)
  private val textSimplifier: YiddishTextSimpifier = YiddishTextSimpifier()

  private val config = ConfigFactory.load().getConfig("jochre.search.yi")
  override val queryFindReplacePairs = config
    .getConfigList("query-replacements")
    .asScala
    .map(c => c.getString("find").r -> c.getString("replace"))
    .map { case (find, replace) =>
      log.info(f"Added query replacement: FIND $find REPLACE $replace")
      find -> replace
    }
    .toSeq

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

  private val punctuationAndNotRegex =
    raw"(?U)\p{Punct}[^\p{Punct}]|[^\p{Punct}]\p{Punct}".r

  private val quoteRegex = raw"""(?U)[‛“'"’]""".r
  private val abbreviationRegex = raw"""(?U)\w+[‛“'"’]\w+""".r

  private val dotRegex = raw"""(?U)\.""".r
  private val decimalNumberRegex = raw"""(?U)\d+\.\d+""".r
  private val punctuationSplitter = raw"""(?U)((?<=\p{Punct}+)|(?=\p{Punct}+))"""
  override def breakWord(word: Word): Seq[Word] = {
    if (punctuationAndNotRegex.findFirstIn(word.content).isDefined) {
      val parts = word.content.split(punctuationSplitter)
      val contentTriplets = (parts :+ "" :+ "")
        .lazyZip("" +: parts :+ "")
        .lazyZip("" +: "" +: parts)
        .toSeq
      val abbreviationIndexes = contentTriplets.zipWithIndex.flatMap { case ((next, current, prev), i) =>
        Option.when(
          (quoteRegex.matches(current) && abbreviationRegex.matches(
            f"$prev$current$next"
          )) ||
            (dotRegex.matches(current) && decimalNumberRegex.matches(
              f"$prev$current$next"
            ))
        )(i - 1)
      }.toSet

      val correctedParts = parts.zipWithIndex.foldLeft(Seq.empty[String]) { case (newParts, (part, i)) =>
        if (newParts.nonEmpty && (abbreviationIndexes.contains(i) || abbreviationIndexes.contains(i - 1))) {
          newParts.init :+ (f"${newParts.last}$part")
        } else {
          newParts :+ part
        }
      }

      val totalLength = word.content.length.toDouble
      val width = word.rectangle.width
      val height = word.rectangle.height
      val left = word.rectangle.left
      val top = word.rectangle.top
      val right = word.rectangle.right

      val (words, _) = correctedParts.foldLeft(Seq.empty[Word] -> right) { case ((words, currentRight), part) =>
        val myWidth = Math.round((part.length.toDouble / totalLength) * width.toDouble).toInt
        val alternatives = getAlternatives(part)

        val word = Word(
          content = part,
          rectangle = Rectangle(currentRight - myWidth, top, myWidth, height),
          glyphs = Seq.empty,
          alternatives = alternatives,
          confidence = 1.0
        )
        (words :+ word) -> (currentRight - myWidth)
      }
      // Ensure the final width covers the entire width (it my be off by 1 due to rounding)
      val lastWord = words.last
      words.init :+ lastWord.copy(rectangle =
        lastWord.rectangle.copy(left = left, width = lastWord.rectangle.width + (lastWord.rectangle.left - left))
      )
    } else {
      Seq(word)
    }
  }

  override def normalizeText(text: String): String = textSimplifier.simplify(text)

  val live: ZLayer[Any, Throwable, LanguageSpecificFilters] = ZLayer.succeed(YiddishFilters)
}
