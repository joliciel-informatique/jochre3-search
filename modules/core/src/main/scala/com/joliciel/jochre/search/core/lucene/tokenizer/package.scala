package com.joliciel.jochre.search.core.lucene

package object tokenizer {
  private[tokenizer] object TokenTypes {
    val ALTERNATIVE_TYPE = "alternative"
    val NEWLINE_TYPE = "newline"
    val PAGE_TYPE = "page"
    val DOC_REF_TYPE_PREFIX = "docRef:"
  }
}
