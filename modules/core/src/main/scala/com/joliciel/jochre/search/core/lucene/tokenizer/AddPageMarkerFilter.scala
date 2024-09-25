package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.{IndexingHelper, PAGE_TOKEN}
import org.apache.lucene.analysis.tokenattributes.{
  CharTermAttribute,
  OffsetAttribute,
  PositionIncrementAttribute,
  TypeAttribute
}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}
import org.apache.lucene.util.AttributeSource
import org.slf4j.LoggerFactory

private[lucene] class AddPageMarkerFilter(input: TokenStream, indexingHelper: IndexingHelper)
    extends TokenFilter(input) {
  private val log = LoggerFactory.getLogger(getClass)

  private val termAttr = addAttribute(classOf[CharTermAttribute])
  private val posIncAttr = addAttribute(classOf[PositionIncrementAttribute])
  private val offsetAttr = addAttribute(classOf[OffsetAttribute])
  private val typeAttr = addAttribute(classOf[TypeAttribute])

  private var isNewPage: Boolean = false
  private var attributeState: AttributeSource.State = _

  final override def incrementToken: Boolean = {
    if (isNewPage) {
      clearAttributes()
      restoreState(attributeState)
      posIncAttr.setPositionIncrement(0)
      isNewPage = false
      true
    } else if (input.incrementToken()) {
      val tokenType = typeAttr.`type`()
      if (tokenType.startsWith(TokenTypes.DOC_REF_TYPE_PREFIX)) {
        val refValue = tokenType.substring(TokenTypes.DOC_REF_TYPE_PREFIX.length)
        val ref = DocReference(refValue)
        val offset = offsetAttr.startOffset()
        isNewPage = indexingHelper.getDocumentInfo(ref).pageOffsets.contains(offset)
        if (isNewPage) {
          if (log.isDebugEnabled) {
            log.debug(f"Found new page for doc ${ref.ref} at offset $offset, word ${termAttr.toString}")
          }
          attributeState = captureState()
          termAttr.copyBuffer(PAGE_TOKEN.toCharArray, 0, PAGE_TOKEN.size)
          typeAttr.setType(TokenTypes.PAGE_TYPE)
        }
      }
      true
    } else {
      isNewPage = false
      false
    }
  }
}
