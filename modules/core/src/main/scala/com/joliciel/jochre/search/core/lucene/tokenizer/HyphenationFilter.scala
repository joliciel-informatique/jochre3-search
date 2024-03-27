package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.IndexingHelper
import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, OffsetAttribute, TypeAttribute}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}
import org.apache.lucene.util.AttributeSource
import org.slf4j.LoggerFactory

private[lucene] class HyphenationFilter(input: TokenStream, indexingHelper: IndexingHelper) extends TokenFilter(input) {
  private val log = LoggerFactory.getLogger(getClass)

  private val termAttr = addAttribute(classOf[CharTermAttribute])
  private val offsetAttr = addAttribute(classOf[OffsetAttribute])
  private val typeAttr = addAttribute(classOf[TypeAttribute])

  private var attributeState: AttributeSource.State = _

  final override def incrementToken: Boolean = {
    if (input.incrementToken()) {
      val tokenType = typeAttr.`type`()
      if (tokenType.startsWith(TokenTypes.DOC_REF_TYPE_PREFIX)) {
        val refValue = tokenType.substring(TokenTypes.DOC_REF_TYPE_PREFIX.length)
        val ref = DocReference(refValue)
        val offset = offsetAttr.startOffset()
        val hyphenated = indexingHelper.getDocumentInfo(ref).hyphenatedWordOffsets.contains(offset)
        if (hyphenated) {
          if (log.isDebugEnabled) log.debug(f"In doc ${ref.ref} word at offset $offset is hyphenated")
          attributeState = captureState()
          // We combine this word with the hyphen and the following word, so we continue incrementing the token
          if (input.incrementToken()) {
            val hyphenWord = termAttr.toString
            val hyphenEndOffset = offsetAttr.endOffset()
            clearAttributes()
            restoreState(attributeState)
            val firstWord = termAttr.toString
            val firstWordStartOffset = offsetAttr.startOffset()
            val withHyphenWord = firstWord + hyphenWord
            if (log.isDebugEnabled)
              log.debug(f"First word: $firstWord. Hyphen word: $hyphenWord. With hyphen: $withHyphenWord")
            termAttr.copyBuffer(withHyphenWord.toCharArray, 0, withHyphenWord.size)
            offsetAttr.setOffset(firstWordStartOffset, hyphenEndOffset)
            attributeState = captureState()
            if (input.incrementToken()) {
              val secondWord = termAttr.toString
              val secondWordEndOffset = offsetAttr.endOffset()
              clearAttributes()
              restoreState(attributeState)
              val firstWord = termAttr.toString
              val firstWordStartOffset = offsetAttr.startOffset()
              val fullWord = firstWord.substring(0, firstWord.length - 1) + secondWord
              if (log.isDebugEnabled)
                log.debug(f"First word: $firstWord. Second word: $secondWord. Full word: $fullWord")
              termAttr.copyBuffer(fullWord.toCharArray, 0, fullWord.size)
              offsetAttr.setOffset(firstWordStartOffset, secondWordEndOffset)
            } else {
              // Do nothing, there's nothing to hyphenate with
            }
          } else {
            // Do nothing, there's no hyphen
          }
        }
      }
      true
    } else {
      false
    }
  }
}
