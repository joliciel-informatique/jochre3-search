package com.joliciel.jochre.search.core.lucene.tokenizer

import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.tokenattributes.{
  CharTermAttribute,
  KeywordAttribute,
  OffsetAttribute,
  PositionIncrementAttribute
}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}
import org.apache.lucene.util.AttributeSource
import org.slf4j.LoggerFactory

import java.util.Locale
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

private[lucene] class RegexTokenizerFilter(input: TokenStream, regex: Regex) extends TokenFilter(input) {
  private val log = LoggerFactory.getLogger(getClass)

  private val termAttr = addAttribute(classOf[CharTermAttribute])
  private val posIncAttr = addAttribute(classOf[PositionIncrementAttribute])
  private val offsetAttr = addAttribute(classOf[OffsetAttribute])
  private val keywordAttr = addAttribute(classOf[KeywordAttribute])

  private var splits: Seq[CharSequence] = Seq.empty
  private var currentSplit: Int = 0
  private var currentStart: Int = _

  private var attributeState: AttributeSource.State = _

  final override def incrementToken: Boolean = {
    if (currentSplit < splits.length) {
      handleNextSplit()
    } else {
      handleNewToken()
    }
  }

  private def handleNewToken(): Boolean = {
    val hasToken = input.incrementToken()

    if (hasToken && !keywordAttr.isKeyword) {
      attributeState = captureState()
      val term = termAttr.subSequence(0, termAttr.length)
      currentSplit = 0
      splits = split(term, Seq.empty)
      splits match {
        case Nil => // this should never happen
        case _ +: Nil => // single split, no need to handle it as a split
          splits = Seq.empty
        case _ +: _ => // handle one word at a time
          if (log.isTraceEnabled) log.trace(f"splits: $splits")
          currentStart = offsetAttr.startOffset()
          handleNextSplit()
      }
    }
    hasToken
  }

  private def handleNextSplit(): Boolean = {
    val currentPositionIncrement = posIncAttr.getPositionIncrement
    clearAttributes()
    restoreState(attributeState)
    val subTerm = splits(currentSplit)
    val firstSplit = currentSplit == 0
    currentSplit += 1
    termAttr.setEmpty()
    termAttr.append(subTerm)
    offsetAttr.setOffset(currentStart, currentStart + subTerm.length)
    currentStart = currentStart + subTerm.length
    if (firstSplit) {
      posIncAttr.setPositionIncrement(currentPositionIncrement)
    } else {
      posIncAttr.setPositionIncrement(1)
    }

    true
  }

  private def split(term: CharSequence, acc: Seq[CharSequence]): Seq[CharSequence] =
    regex.findFirstMatchIn(term) match {
      case Some(regexMatch) =>
        val splits = Seq(
          term.subSequence(0, regexMatch.start),
          term.subSequence(regexMatch.start, regexMatch.end),
          term.subSequence(regexMatch.end, term.length())
        ).filter(_.length() > 0)
        splits match {
          case Nil         => acc
          case head +: Nil => acc :+ head
          case _           =>
            // In this case the current match split the string into two or more parts.
            // We retain the portion before the match and the match itself as whole separate tokens.
            // We then apply the regex again to the final part, in case there are more tokens to split off
            acc ++ splits.init ++ split(splits.last, Seq.empty)
        }
      case None => acc :+ term
    }
}

object RegexTokenizerFilter {
  def apply(input: TokenStream, locale: Locale): RegexTokenizerFilter = {
    val config = ConfigFactory.load().getConfig(f"jochre.search")
    val configKey = f"${locale.getLanguage}.tokenizer"

    val regexes = config
      .getStringList(configKey)
      .asScala
      .map(r => f"($r)")
    val regex = f"(?U)${regexes.mkString("|")}".r

    new RegexTokenizerFilter(input, regex)
  }
}
