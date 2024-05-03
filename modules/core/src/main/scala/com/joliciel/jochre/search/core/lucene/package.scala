package com.joliciel.jochre.search.core

package object lucene {
  private[lucene] case class Token(value: String, start: Int, end: Int, score: Float)

  case class IndexTerm(text: String, start: Int, end: Int, position: Int) {
    def token: Token = Token(text, start, end, 1.0f)
  }

  private[lucene] val PAGE_TOKEN = "⚅"
  private[lucene] val NEWLINE_TOKEN = "⏎"
}
