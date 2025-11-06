package com.joliciel.jochre.search.core.lucene

import org.apache.lucene.search.Query
import org.apache.lucene.queries.spans.SpanQuery
import org.apache.lucene.search.PhraseQuery
import org.apache.lucene.queries.spans.SpanNearQuery
import org.apache.lucene.queries.spans.{SpanNearQuery, SpanOrQuery, SpanQuery, SpanTermQuery}
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.highlight.{Highlighter, QueryScorer, SimpleHTMLFormatter, TextFragment}
import org.apache.lucene.search.{
  BooleanQuery,
  MatchNoDocsQuery,
  MultiPhraseQuery,
  PhraseQuery,
  Query,
  SynonymQuery,
  TermQuery
}
import com.joliciel.jochre.search.core.IndexField

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._
import org.apache.lucene.search.WildcardQuery
import org.apache.lucene.queries.spans.SpanMultiTermQueryWrapper
import org.apache.lucene.search.PrefixQuery
import org.apache.lucene.search.RegexpQuery
import org.apache.lucene.search.FuzzyQuery
import org.slf4j.LoggerFactory
import org.apache.lucene.index.Term
import org.apache.lucene.queryparser.charstream.CharStream
import org.apache.lucene.search.BooleanClause

object QueryExtensions {

  private val log = LoggerFactory.getLogger(getClass)

  private def buildSpanQuery(query: Query, field: String): Option[SpanQuery] = query match {
    case query: SpanQuery =>
      Option.when(query.getField() == field) {
        query
      }
    case query: PhraseQuery =>
      Option.when(query.getField == field) {
        val hasWildcard = query.getPositions.length > 0 && query.getPositions
          .zip(query.getPositions.tail)
          .map { case (p1, p2) => p2 - p1 }
          .exists(_ > 1)
        val strictOrder = hasWildcard || query.getSlop == 0
        val builder = new SpanNearQuery.Builder(field, strictOrder)
        builder.setSlop(query.getSlop)
        val termsAndPositions = query.getTerms.zip(query.getPositions)
        termsAndPositions.foldLeft(0) { case (currentPos, (term, position)) =>
          if (position > currentPos) {
            builder.addGap(position - currentPos)
          }
          val spanQuery = new SpanTermQuery(term)
          builder.addClause(spanQuery)
          position + 1
        }
        builder.build()
      }
    case query: MultiPhraseQuery =>
      val termArrays = query.getTermArrays.toSeq
      Option.when(!termArrays.isEmpty && !termArrays(0).isEmpty && termArrays(0)(0).field() == field) {
        val hasWildcard = query.getPositions.length > 0 && query.getPositions
          .zip(query.getPositions.tail)
          .map { case (p1, p2) => p2 - p1 }
          .exists(_ > 1)
        val strictOrder = hasWildcard || query.getSlop == 0
        val builder = new SpanNearQuery.Builder(field, strictOrder)
        builder.setSlop(query.getSlop)
        val termsAndPositions = termArrays.zip(query.getPositions)
        termsAndPositions.foldLeft(0) { case (currentPos, (terms, position)) =>
          if (position > currentPos) {
            builder.addGap(position - currentPos)
          }
          val termQueries = terms.map(term => new SpanTermQuery(term))
          val synonymQuery = new SpanOrQuery(termQueries.toArray*)
          builder.addClause(synonymQuery)
          position + 1
        }
        builder.build()
      }
    case query: TermQuery =>
      Option.when(query.getTerm.field() == field) {
        new SpanTermQuery(query.getTerm)
      }
    case query: SynonymQuery =>
      Option.when(!query.getTerms.isEmpty && query.getTerms.get(0).field() == field) {
        val termQueries = query.getTerms.asScala.map(term => new SpanTermQuery(term))
        new SpanOrQuery(termQueries.toArray*)
      }
    case query: WildcardQuery =>
      Option.when(query.getField() == field) {
        new SpanMultiTermQueryWrapper[WildcardQuery](query)
      }
    case query: PrefixQuery =>
      Option.when(query.getField() == field) {
        new SpanMultiTermQueryWrapper[PrefixQuery](query)
      }
    case query: RegexpQuery =>
      Option.when(query.getField() == field) {
        new SpanMultiTermQueryWrapper[RegexpQuery](query)
      }
    case query: FuzzyQuery =>
      Option.when(query.getField() == field) {
        new SpanMultiTermQueryWrapper[FuzzyQuery](query)
      }
    case query: BooleanQuery =>
      val allClauses = query.clauses().asScala
      val positiveClauses = allClauses
        .filter(clause => clause.getOccur == Occur.SHOULD || clause.getOccur == Occur.MUST)
        .flatMap(clause => clause.getQuery.toSpanQuery(field))

      Option.when(positiveClauses.nonEmpty) {
        if (positiveClauses.length == 1) {
          positiveClauses.head
        } else {
          new SpanOrQuery(positiveClauses.toArray*)
        }
      }
    case other =>
      if (log.isDebugEnabled) log.debug(f"Cannot convert ${other.getClass.getName}: $other to span query")
      None
  }

  extension (query: Query) {
    def toSpanQuery(field: String): Option[SpanQuery] = QueryExtensions.buildSpanQuery(query, field)

    def extractClauses(): Seq[Query] = query match {
      case booleanQuery: BooleanQuery =>
        booleanQuery
          .clauses()
          .asScala
          .map { clause =>
            clause.getQuery() match {
              case clauseQuery: BooleanQuery if clauseQuery.clauses().size() == 1 =>
                clauseQuery.clauses().asScala.head.getQuery()
              case other => other
            }
          }
          .toVector
      case _ => Seq(query)
    }
  }
}
