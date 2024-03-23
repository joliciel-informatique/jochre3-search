package com.joliciel.jochre.search.core.lucene.highlight

import com.joliciel.jochre.search.core.lucene.NEWLINE_TOKEN
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.search.highlight.Fragmenter

private[lucene] class RowFragmenter() extends Fragmenter {
  private var termAtt: CharTermAttribute = _

  override def start(originalText: String, tokenStream: TokenStream): Unit = {
    termAtt = tokenStream.addAttribute(classOf[CharTermAttribute])
  }

  override def isNewFragment: Boolean = {
    val term = new String(termAtt.buffer().slice(0, termAtt.length()))
    val rowStart = term == NEWLINE_TOKEN

    rowStart
  }
}
