package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.ocr.core.model.SpellingAlternative
import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.AlternativeHolder
import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, OffsetAttribute, PositionIncrementAttribute}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}
import org.apache.lucene.util.AttributeSource
import org.slf4j.LoggerFactory

private[lucene] class AddAlternativesFilter(input: TokenStream, alternativeHolder: AlternativeHolder)
    extends TokenFilter(input) {
  private val log = LoggerFactory.getLogger(getClass)

  private val termAttr = addAttribute(classOf[CharTermAttribute])
  private val posIncAttr = addAttribute(classOf[PositionIncrementAttribute])
  private val offsetAttr = addAttribute(classOf[OffsetAttribute])

  private var refOption: Option[DocReference] = None
  private var currentAlternatives: Seq[SpellingAlternative] = Seq.empty
  private var attributeState: AttributeSource.State = _

  final override def incrementToken: Boolean = {
    if (refOption.isEmpty) {
      if (input.incrementToken()) {
        // Read the document reference, and skip it downstream.
        refOption = Some(DocReference(termAttr.toString))
        incrementToken()
      } else {
        false
      }
    } else if (currentAlternatives.nonEmpty) {
      clearAttributes()
      restoreState(attributeState)
      val alternative = currentAlternatives.head
      currentAlternatives = currentAlternatives.tail
      termAttr.copyBuffer(alternative.content.toCharArray, 0, alternative.content.size)
      posIncAttr.setPositionIncrement(0)
      true
    } else if (input.incrementToken()) {
      val ref = refOption.get
      val offset = offsetAttr.startOffset()
      currentAlternatives = alternativeHolder.getAlternatives(ref, offset)
      if (currentAlternatives.nonEmpty) {
        log.debug(f"Found alternatives at offset $offset: ${currentAlternatives.map(_.content).mkString(", ")}")
        attributeState = captureState()
      }
      true
    } else {
      refOption.foreach { ref =>
        alternativeHolder.removeAlternatives(ref)
        refOption = None
      }
      false
    }
  }
}
