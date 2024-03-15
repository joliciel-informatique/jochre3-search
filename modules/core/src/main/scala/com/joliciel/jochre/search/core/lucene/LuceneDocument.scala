package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.DocReference
import org.apache.lucene.document.Document

private[lucene] class LuceneDocument(protected val indexSearcher: JochreSearcher, val luceneId: Int) {
  lazy val doc: Document = indexSearcher.storedFields.document(luceneId)
  lazy val ref: DocReference = DocReference(doc.get(LuceneField.Id.name))
}
