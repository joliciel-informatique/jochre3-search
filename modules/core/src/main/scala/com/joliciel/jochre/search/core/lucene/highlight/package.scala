package com.joliciel.jochre.search.core.lucene

import org.apache.lucene.search.highlight.{TextFragment, TextFragmentWrapper}

package object highlight {
  case class HighlightFragment(start: Int, end: Int, page: Int, tokens: Seq[Token]) {
    val score = tokens.map(_.score).sum
  }

  case class PreformattedHighlightFragment(text: String, score: Float, index: Int, start: Int, end: Int)
      extends Ordered[PreformattedHighlightFragment] {
    import scala.math.Ordered.orderingToOrdered

    // Note reverse natural ordering for score
    def compare(that: PreformattedHighlightFragment): Int = (that.score, this.index) compare (this.score, that.index)
  }

  object PreformattedHighlightFragment {
    def apply(textFragment: TextFragment): PreformattedHighlightFragment = {
      val wrapper = new TextFragmentWrapper(textFragment)
      PreformattedHighlightFragment(wrapper.text, wrapper.score, wrapper.index, wrapper.textStartPos, wrapper.textEndPos)
    }
  }
}
