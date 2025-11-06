package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.IndexField
import com.joliciel.jochre.search.core.lucene.QueryExtensions.*
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.queryparser.classic.MultiFieldQueryParser
import org.apache.lucene.search.Query
import org.slf4j.LoggerFactory
import scala.util.matching.Regex
import org.apache.lucene.search.PhraseQuery
import org.apache.lucene.queries.spans.SpanTermQuery
import org.apache.lucene.queries.spans.SpanQuery
import org.apache.lucene.queries.spans.SpanNearQuery
import org.apache.lucene.queries.spans.SpanOrQuery
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.queries.spans.FieldMaskingSpanQuery
import com.typesafe.config.ConfigFactory

private[core] class JochreMultiFieldQueryParser(
    fields: Seq[IndexField],
    termAnalyzer: Analyzer,
    phraseAnalyzer: Analyzer,
    analyzerGroup: AnalyzerGroup
) extends MultiFieldQueryParser(fields.map(_.fieldName).toArray, termAnalyzer)
    with LuceneUtilities {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.search")
  private val allowLeadingWildcard = config.getBoolean("allow-leading-wildcard")
  private val allowLeadingWildcardInPhrases = config.getBoolean("allow-leading-wildcard-in-phrases")

  private val wildcardPlaceholder = "wildcard42"

  this.setAllowLeadingWildcard(allowLeadingWildcard)

  private val innerPhraseAnalyzers = fields.map { field =>
    val parser = new QueryParser(field.fieldName, phraseAnalyzer)
    // Behind the scenes, wildcard queries are converted into boolean queries with all possible terms that match
    // So allowing leading wildcard is almost guaranteed to result in "TooManyNestedClauses: Query contains too many nested clauses"
    parser.setAllowLeadingWildcard(allowLeadingWildcardInPhrases)
    field.fieldName -> parser
  }.toMap

  override def getFieldQuery(field: String, queryText: String, slop: Int): Query = {
    // This override is only called for phrase queries
    // Instead of building a phrase query, it builds a SpanNearQuery,
    // which allows us to integrate wildcard queries inside the phrase
    if (log.isDebugEnabled) log.debug(f"Analyzing quoted phrase: $queryText")

    val myFields = if (Option(field).isEmpty) {
      fields.map(_.fieldName)
    } else {
      Seq(field)
    }

    // Parse query text as if it was outside of a phrase
    val normalizedText = analyzerGroup.languageSpecificFilters.map(_.normalizeText(queryText)).getOrElse(queryText)
    val textWithWildcards = normalizedText.replaceAll("""\*""", wildcardPlaceholder)

    val firstField = myFields.head
    val spanQueries = myFields.flatMap { myField =>
      val parser = innerPhraseAnalyzers.get(myField).get

      val phraseQuery = parser.parse(textWithWildcards)

      val spanQueries = tokenizeString(textWithWildcards, analyzerGroup.forWords)
        .map(_.value)
        .zipWithIndex
        .map { case (token, i) =>
          log.debug(f"Token $i: $token")
          token match {
            case token if token == wildcardPlaceholder => None
            case _ =>
              val fixedToken = token.replaceAll(wildcardPlaceholder, "*")
              val query = parser.parse(fixedToken)
              query.toSpanQuery(myField).map(spanQuery => spanQuery -> i)
          }
        }

      val spanQueriesWithPositions = spanQueries.flatten

      val hasWildcard = spanQueries.exists(element => element.isEmpty)
      val strictOrder = hasWildcard || slop == 0
      Option.when(spanQueriesWithPositions.nonEmpty) {
        if (spanQueriesWithPositions.size == 1) {
          spanQueriesWithPositions.head._1
        } else {
          val builder = new SpanNearQuery.Builder(myField, strictOrder)
          builder.setSlop(slop)

          spanQueriesWithPositions.foldLeft(0) { case (currentPos, (clause, position)) =>
            if (position > currentPos) {
              builder.addGap(position - currentPos)
            }
            builder.addClause(clause)
            position + 1
          }

          val spanQuery = builder.build()
          if (log.isDebugEnabled) log.debug(f"After conversion to span query: $spanQuery")
          if (myField != firstField) {
            new FieldMaskingSpanQuery(spanQuery, firstField)
          } else {
            spanQuery
          }
        }
      }
    }

    if (spanQueries.size == 1) {
      spanQueries.head
    } else {
      new SpanOrQuery(spanQueries*)
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
