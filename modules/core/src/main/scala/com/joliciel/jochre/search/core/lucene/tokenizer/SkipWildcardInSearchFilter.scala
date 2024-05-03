package com.joliciel.jochre.search.core.lucene.tokenizer

import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, PositionIncrementAttribute}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}
import org.slf4j.LoggerFactory

private[lucene] class SkipWildcardInSearchFilter(input: TokenStream) extends TokenFilter(input) {
  private val log = LoggerFactory.getLogger(getClass)
  val config = ConfigFactory.load().getConfig(f"jochre.search")
  val wildcardMarker = config.getString("wildcard-marker")
  private val charTermAttr: CharTermAttribute = addAttribute(classOf[CharTermAttribute])
  private val positionIncrementAttr: PositionIncrementAttribute = addAttribute(classOf[PositionIncrementAttribute])

  final override def incrementToken(): Boolean = {
    this.getNextToken(0)
  }

  def getNextToken(additionalIncrement: Int): Boolean = {
    val result = input.incrementToken()
    if (result) {
      if (charTermAttr.toString == wildcardMarker) {
        this.getNextToken(additionalIncrement + 1)
      } else {
        if (additionalIncrement > 0) {
          val currentIncrement = positionIncrementAttr.getPositionIncrement
          if (log.isDebugEnabled)
            log.debug(
              f"Found wildcard marker, increment was ${currentIncrement}, now ${currentIncrement + additionalIncrement}"
            )
          positionIncrementAttr.setPositionIncrement(currentIncrement + additionalIncrement)
        }
      }
    }
    result
  }
}
