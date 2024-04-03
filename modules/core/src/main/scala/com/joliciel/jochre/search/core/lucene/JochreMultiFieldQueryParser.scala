package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.IndexField
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.queryparser.classic.MultiFieldQueryParser
import org.apache.lucene.search.Query
import org.slf4j.LoggerFactory

private[core] class JochreMultiFieldQueryParser(
    fields: Seq[IndexField],
    termAnalyzer: Analyzer,
    phraseAnalyzer: Analyzer
) extends MultiFieldQueryParser(fields.map(_.entryName).toArray, phraseAnalyzer)
    with LuceneUtilities {
  private val log = LoggerFactory.getLogger(getClass)

  override def newFieldQuery(analyzer: Analyzer, field: String, queryText: String, quoted: Boolean): Query = {
    if (quoted) {
      if (log.isDebugEnabled) log.debug(f"Analyzing quoted phrase: $queryText")
      super.newFieldQuery(phraseAnalyzer, field, queryText, quoted)
    } else {
      if (log.isDebugEnabled) log.debug(f"Analyzing unquoted text: $queryText")
      super.newFieldQuery(termAnalyzer, field, queryText, quoted)
    }
  }
}
