package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.DocReference

private[lucene] case class IndexingHelper() {
  private var docInfoMap: Map[DocReference, DocumentIndexInfo] = Map.empty
  def addDocumentInfo(ref: DocReference, docInfo: DocumentIndexInfo): Unit =
    docInfoMap = docInfoMap + (ref -> docInfo)

  def removeDocumentInfo(ref: DocReference): Unit =
    docInfoMap = docInfoMap - ref

  def getDocumentInfo(ref: DocReference): DocumentIndexInfo =
    docInfoMap.get(ref).getOrElse(throw new Exception(f"No doc info for ${ref.ref}"))
}
