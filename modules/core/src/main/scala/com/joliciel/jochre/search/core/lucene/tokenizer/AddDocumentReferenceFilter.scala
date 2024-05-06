package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.IndexingHelper
import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, TypeAttribute}
import org.apache.lucene.analysis.{TokenFilter, TokenStream}
import org.slf4j.LoggerFactory

/** Expects the document reference to be the first token in this token stream. Reads it and skips it for downstream
  * token filters, storing the document reference in the "type" attribute.
  */
class AddDocumentReferenceFilter(input: TokenStream, indexingHelper: IndexingHelper) extends TokenFilter(input) {
  private val log = LoggerFactory.getLogger(getClass)
  private val termAttr = addAttribute(classOf[CharTermAttribute])
  private val typeAttr = addAttribute(classOf[TypeAttribute])

  private var refOption: Option[DocReference] = None

  override def incrementToken(): Boolean = {
    if (refOption.isEmpty) {
      if (input.incrementToken()) {
        // Read the document reference, and skip it downstream.
        val docRef = termAttr.toString
        refOption = Some(DocReference(docRef))
        log.debug(s"Found doc ref $docRef")
        incrementToken()
      } else {
        false
      }
    } else if (input.incrementToken()) {
      typeAttr.setType(f"${TokenTypes.DOC_REF_TYPE_PREFIX}${refOption.get.ref}")
      true
    } else {
      refOption = None
      false
    }
  }
}
