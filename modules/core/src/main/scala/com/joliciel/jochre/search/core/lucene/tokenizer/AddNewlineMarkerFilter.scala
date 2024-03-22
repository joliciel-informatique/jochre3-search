package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.{IndexingHelper, NEWLINE_TOKEN}
import org.apache.lucene.analysis.tokenattributes.{
  CharTermAttribute,
  OffsetAttribute,
  PositionIncrementAttribute,
  TypeAttribute
}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}
import org.apache.lucene.util.AttributeSource
import org.slf4j.LoggerFactory

private[lucene] class AddNewlineMarkerFilter(input: TokenStream, indexingHelper: IndexingHelper)
    extends TokenFilter(input) {
  private val log = LoggerFactory.getLogger(getClass)

  private val termAttr = addAttribute(classOf[CharTermAttribute])
  private val posIncAttr = addAttribute(classOf[PositionIncrementAttribute])
  private val offsetAttr = addAttribute(classOf[OffsetAttribute])
  private val typeAttr = addAttribute(classOf[TypeAttribute])

  private var isNewline: Boolean = false
  private var attributeState: AttributeSource.State = _

  final override def incrementToken: Boolean = {
    if (isNewline) {
      clearAttributes()
      restoreState(attributeState)
      posIncAttr.setPositionIncrement(0)
      isNewline = false
      true
    } else if (input.incrementToken()) {
      val tokenType = typeAttr.`type`()
      if (tokenType.startsWith(DOC_REF_TYPE_PREFIX)) {
        val refValue = tokenType.substring(DOC_REF_TYPE_PREFIX.length)
        val ref = DocReference(refValue)
        val offset = offsetAttr.startOffset()
        isNewline = indexingHelper.getDocumentInfo(ref).newlineOffsets.contains(offset)
        if (isNewline) {
          log.debug(f"Found newline for doc ${ref.ref}  at offset $offset")
          attributeState = captureState()
          termAttr.copyBuffer(NEWLINE_TOKEN.toCharArray, 0, NEWLINE_TOKEN.size)
          typeAttr.setType(NEWLINE_TYPE)
        }
      }
      true
    } else {
      false
    }
  }
}
