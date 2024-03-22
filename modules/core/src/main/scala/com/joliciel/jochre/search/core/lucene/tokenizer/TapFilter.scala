package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.search.core.lucene.Token
import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, OffsetAttribute, PositionIncrementAttribute}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}
import org.slf4j.Logger

/** Useful for debug
  *
  * @param input
  */
private[lucene] class TapFilter(input: TokenStream, log: Logger, logName: String = "") extends TokenFilter(input) {
  private val charTermAttr: CharTermAttribute = addAttribute(classOf[CharTermAttribute])
  private val positionIncrementAttr: PositionIncrementAttribute = addAttribute(classOf[PositionIncrementAttribute])
  private val offsetAttr: OffsetAttribute = addAttribute(classOf[OffsetAttribute])

  final override def incrementToken(): Boolean = {
    val result = input.incrementToken()
    if (result) {
      val string = charTermAttr.toString
      val token = Token(string, offsetAttr.startOffset(), offsetAttr.endOffset(), 1.0f)
      if (log.isTraceEnabled) {
        log.trace(f"$logName: $string = ${token}, posIncr: ${positionIncrementAttr.getPositionIncrement}")
      }
    }
    result
  }
}
