package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.IndexField
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.joliciel.jochre.search.core.WithTestIndex
import org.apache.lucene.queries.spans.SpanOrQuery
import org.apache.lucene.queries.spans.SpanNearQuery
import org.slf4j.LoggerFactory

class JochreMultiFieldQueryParserTest extends AnyFlatSpec with Matchers with LuceneUtilities with WithTestIndex {
  private val log = LoggerFactory.getLogger(getClass)

  "a parser" should "correctly parse a phrase query" in {
    val parser = new JochreMultiFieldQueryParser(
      Seq(IndexField.Text),
      analyzerGroup.forSearch,
      analyzerGroup.forSearchPhrases,
      analyzerGroup
    )
    val query = parser.parse("\"באָבע מעשה\"")
    log.debug(f"query ${query}")
    query match {
      case nearQuery: SpanNearQuery => // no problem
      case other                    => throw new Exception(f"Unexpected query type: $other")
    }
  }
}
