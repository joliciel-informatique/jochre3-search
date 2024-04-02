package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.IndexingHelper
import org.apache.lucene.analysis.tokenattributes.{
  CharTermAttribute,
  OffsetAttribute,
  PositionIncrementAttribute,
  TypeAttribute
}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}
import org.apache.lucene.util.AttributeSource
import org.slf4j.LoggerFactory

private[lucene] class HyphenationFilter(input: TokenStream, indexingHelper: IndexingHelper) extends TokenFilter(input) {
  private val log = LoggerFactory.getLogger(getClass)

  private val termAttr = addAttribute(classOf[CharTermAttribute])
  private val posIncAttr = addAttribute(classOf[PositionIncrementAttribute])
  private val offsetAttr = addAttribute(classOf[OffsetAttribute])
  private val typeAttr = addAttribute(classOf[TypeAttribute])

  private var attributeState: AttributeSource.State = _
  private var setAside: Seq[AttributeSource.State] = Seq.empty

  final override def incrementToken: Boolean = {
    if (setAside.nonEmpty) {
      clearAttributes()
      restoreState(setAside.head)
      // Since we're inside a hyphenation, we always set the position increment to 0
      posIncAttr.setPositionIncrement(0)

      setAside = setAside.tail
      true
    } else if (input.incrementToken()) {
      val tokenType = typeAttr.`type`()
      if (tokenType.startsWith(TokenTypes.DOC_REF_TYPE_PREFIX)) {
        val refValue = tokenType.substring(TokenTypes.DOC_REF_TYPE_PREFIX.length)
        val ref = DocReference(refValue)
        val offset = offsetAttr.startOffset()
        val hyphenated = indexingHelper.getDocumentInfo(ref).hyphenatedWordOffsets.contains(offset)
        if (hyphenated) {
          if (log.isDebugEnabled) log.debug(f"In doc ${ref.ref} word at offset $offset is hyphenated")
          // First concatenate the hyphen
          concatenateOrSetAsideNextToken(false)
          // Then concatenate the next word
          concatenateOrSetAsideNextToken(true)
        }
      }
      true
    } else {
      false
    }
  }

  private def concatenateOrSetAsideNextToken(addSecondWordContent: Boolean): Unit = {
    attributeState = captureState()
    // We combine this word with the hyphen and the following word, so we continue incrementing the token
    var takeNext = true
    while (takeNext) {
      if (input.incrementToken()) {
        val tokenType = typeAttr.`type`()
        if (tokenType.startsWith(TokenTypes.DOC_REF_TYPE_PREFIX)) {
          val secondWord = termAttr.toString
          val secondWordEndOffset = offsetAttr.endOffset()
          clearAttributes()
          restoreState(attributeState)
          val firstWord = termAttr.toString
          val firstWordStartOffset = offsetAttr.startOffset()
          val fullWord = if (addSecondWordContent) {
            firstWord + secondWord
          } else {
            firstWord
          }
          if (log.isDebugEnabled)
            log.debug(f"First word: $firstWord. Second word: $secondWord. Full word: $fullWord")
          termAttr.copyBuffer(fullWord.toCharArray, 0, fullWord.size)
          offsetAttr.setOffset(firstWordStartOffset, secondWordEndOffset)
          takeNext = false
        } else {
          setAside = setAside :+ captureState()
        }
      }
    }
  }
}
