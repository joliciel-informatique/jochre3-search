package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.ocr.core.model.SpellingAlternative
import com.joliciel.jochre.search.core.DocReference

private[lucene] case class AlternativeHolder () {
  var alternativeMap: Map[DocReference, Map[Int, Seq[SpellingAlternative]]] = Map.empty
  def addAlternatives(ref: DocReference, alternatives: Map[Int, Seq[SpellingAlternative]]): Unit =
    alternativeMap = alternativeMap + (ref -> alternatives)

  def removeAlternatives(ref: DocReference): Unit =
    alternativeMap = alternativeMap - ref

  def getAlternatives(ref: DocReference, offset: Int): Seq[SpellingAlternative] = {
    val alternativesForRef = alternativeMap.get(ref).getOrElse(throw new Exception(f"No alternative map for ${ref.ref}"))
    val alternatives = alternativesForRef.getOrElse(offset, Seq.empty)
    alternatives
  }
}
