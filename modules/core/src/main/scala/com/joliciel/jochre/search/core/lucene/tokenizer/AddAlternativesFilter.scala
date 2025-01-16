package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.ocr.core.model.SpellingAlternative
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

private[lucene] class AddAlternativesFilter(input: TokenStream, indexingHelper: IndexingHelper)
    extends TokenFilter(input) {
  private val log = LoggerFactory.getLogger(getClass)

  private val termAttr = addAttribute(classOf[CharTermAttribute])
  private val posIncAttr = addAttribute(classOf[PositionIncrementAttribute])
  private val offsetAttr = addAttribute(classOf[OffsetAttribute])
  private val typeAttr = addAttribute(classOf[TypeAttribute])

  private var currentAlternatives: Seq[SpellingAlternative] = Seq.empty
  private var attributeState: AttributeSource.State = scala.compiletime.uninitialized

  final override def incrementToken: Boolean = {
    if (currentAlternatives.nonEmpty) {
      clearAttributes()
      restoreState(attributeState)
      val alternative = currentAlternatives.head
      currentAlternatives = currentAlternatives.tail
      termAttr.copyBuffer(alternative.content.toCharArray, 0, alternative.content.size)
      posIncAttr.setPositionIncrement(0)
      typeAttr.setType(TokenTypes.ALTERNATIVE_TYPE)
      true
    } else if (input.incrementToken()) {
      val tokenType = typeAttr.`type`()
      if (tokenType.startsWith(TokenTypes.DOC_REF_TYPE_PREFIX)) {
        val refValue = tokenType.substring(TokenTypes.DOC_REF_TYPE_PREFIX.length)
        val ref = DocReference(refValue)
        val offset = offsetAttr.startOffset()
        currentAlternatives = indexingHelper.getDocumentInfo(ref).getAlternatives(offset)
        if (currentAlternatives.nonEmpty) {
          log.debug(f"Found alternatives for doc ${ref.ref} at offset $offset: ${currentAlternatives.map(_.content).mkString(", ")}")
          attributeState = captureState()
        }
      }
      true
    } else {
      currentAlternatives = Seq.empty
      false
    }
  }
}
