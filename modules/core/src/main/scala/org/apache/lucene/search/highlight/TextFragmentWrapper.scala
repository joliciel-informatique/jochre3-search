package org.apache.lucene.search.highlight

/** Placed in lucene package to expose start and end positions.
  */
class TextFragmentWrapper(textFragment: TextFragment) {
  val textStartPos = textFragment.textStartPos
  val textEndPos = textFragment.textEndPos
  val score = textFragment.score
  val index = textFragment.fragNum
  val text = textFragment.toString
}
