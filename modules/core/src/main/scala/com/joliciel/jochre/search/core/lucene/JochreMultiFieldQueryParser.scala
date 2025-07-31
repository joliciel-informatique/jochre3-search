package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.IndexField
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.queryparser.classic.MultiFieldQueryParser
import org.apache.lucene.search.Query
import org.slf4j.LoggerFactory

private[core] class JochreMultiFieldQueryParser(
    fields: Seq[IndexField],
    termAnalyzer: Analyzer,
    phraseAnalyzer: Analyzer,
    analyzerGroup: AnalyzerGroup
) extends MultiFieldQueryParser(fields.map(_.fieldName).toArray, phraseAnalyzer)
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

  override protected def getPrefixQuery(field: String, termStr: String): Query = {
    val normalizedTermStr = analyzerGroup.languageSpecificFilters.map(_.normalizeText(termStr)).getOrElse(termStr)
    super.getPrefixQuery(field, normalizedTermStr)
  }

  override protected def getWildcardQuery(field: String, termStr: String): Query = {
    val normalizedTermStr = analyzerGroup.languageSpecificFilters.map(_.normalizeText(termStr)).getOrElse(termStr)
    super.getWildcardQuery(field, normalizedTermStr)
  }

  override protected def getFuzzyQuery(field: String, termStr: String, minSimilarity: Float): Query = {
    val normalizedTermStr = analyzerGroup.languageSpecificFilters.map(_.normalizeText(termStr)).getOrElse(termStr)
    super.getFuzzyQuery(field, normalizedTermStr, minSimilarity)
  }

  override protected def getRegexpQuery(field: String, termStr: String): Query = {
    val normalizedTermStr = analyzerGroup.languageSpecificFilters.map(_.normalizeText(termStr)).getOrElse(termStr)
    super.getRegexpQuery(field, normalizedTermStr)
  }
}
