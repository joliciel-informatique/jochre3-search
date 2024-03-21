package com.joliciel.jochre.search.core.lucene

import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.queryparser.classic.MultiFieldQueryParser
import org.apache.lucene.search.Query

private[lucene] class JochreMultiFieldQueryParser(
    fields: Seq[LuceneField],
    phraseAnalyzer: Analyzer,
    termAnalyzer: Analyzer
) extends MultiFieldQueryParser(fields.map(_.name).toArray, phraseAnalyzer)
    with LuceneUtilities {

  override def newFieldQuery(analyzer: Analyzer, field: String, queryText: String, quoted: Boolean): Query = {
    if (quoted) {
      super.newFieldQuery(phraseAnalyzer, field, queryText, quoted)
    } else {
      super.newFieldQuery(termAnalyzer, field, queryText, quoted)
    }
  }
}
