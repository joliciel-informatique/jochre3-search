package com.joliciel.jochre.search.core.lucene

import org.apache.lucene.search.highlight.{TextFragment, TextFragmentWrapper}

package object highlight {
  case class HighlightFragment(text: String, score: Float, index: Int, start: Int, end: Int)
      extends Ordered[HighlightFragment] {
    import scala.math.Ordered.orderingToOrdered

    // Note reverse natural ordering for score
    def compare(that: HighlightFragment): Int = (that.score, this.index) compare (this.score, that.index)
  }

  object HighlightFragment {
    def apply(textFragment: TextFragment): HighlightFragment = {
      val wrapper = new TextFragmentWrapper(textFragment)
      HighlightFragment(wrapper.text, wrapper.score, wrapper.index, wrapper.textStartPos, wrapper.textEndPos)
    }
  }
}
