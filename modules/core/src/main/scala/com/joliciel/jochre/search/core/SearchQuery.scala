package com.joliciel.jochre.search.core

import com.joliciel.jochre.search.core.lucene.{AnalyzerGroup, JochreMultiFieldQueryParser}
import org.apache.lucene.document.IntPoint
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.{BooleanQuery, MatchAllDocsQuery, PrefixQuery, Query, TermInSetQuery}
import org.apache.lucene.util.BytesRef

import scala.jdk.CollectionConverters._

case class SearchQuery(criterion: SearchCriterion)

sealed trait SearchCriterion {
  private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query
}

object SearchCriterion {
  case class Contains(fields: Seq[IndexField], queryString: String, strict: Boolean) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      fields.foreach { field =>
        if (!field.isTokenized) {
          throw new WrongFieldTypeException(
            f"Cannot perform Contains on field ${field.entryName} - field not tokenized"
          )
        }
      }
      val parser = if (strict) {
        new JochreMultiFieldQueryParser(
          fields = fields,
          termAnalyzer = analyzerGroup.forStrictSearch,
          phraseAnalyzer = analyzerGroup.forStrictSearchPhrases
        )
      } else {
        new JochreMultiFieldQueryParser(
          fields = fields,
          termAnalyzer = analyzerGroup.forSearch,
          phraseAnalyzer = analyzerGroup.forSearchPhrases
        )
      }
      parser.parse(queryString)
    }
  }

  object Contains {
    def apply(field: IndexField, queryString: String, strict: Boolean = false): Contains =
      Contains(Seq(field), queryString, strict)
  }

  case class ValueIn(field: IndexField, values: Seq[String]) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      if (field.isTokenized) {
        throw new WrongFieldTypeException(f"Cannot perform ValueIn on field ${field.entryName}: field is tokenized")
      }
      new TermInSetQuery(field.entryName, values.map(str => new BytesRef(str)).asJavaCollection)
    }
  }

  case class StartsWith(field: IndexField, prefix: String) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      if (field.isTokenized) {
        throw new WrongFieldTypeException(f"Cannot perform StartsWith on field ${field.entryName}: field is tokenized")
      }
      val prefixQuery = new PrefixQuery(new Term(IndexField.Author.entryName, prefix))
      prefixQuery
    }
  }

  case class GreaterThanOrEqualTo(field: IndexField, value: Int) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      if (field.kind != FieldKind.Integer) {
        throw new WrongFieldTypeException(f"Field ${field.entryName} is not an integer, cannot do GreaterThanOrEqualTo")
      }
      IntPoint.newRangeQuery(field.entryName, value, Int.MaxValue)
    }
  }

  case class LessThanOrEqualTo(field: IndexField, value: Int) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      if (field.kind != FieldKind.Integer) {
        throw new WrongFieldTypeException(f"Field ${field.entryName} is not an integer, cannot do LessThanOrEqualTo")
      }
      IntPoint.newRangeQuery(field.entryName, Int.MinValue, value)
    }
  }

  case class Not(criterion: SearchCriterion) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      val builder = new BooleanQuery.Builder()
      builder.add(new MatchAllDocsQuery(), Occur.SHOULD)
      builder.add(criterion.toLuceneQuery(analyzerGroup), Occur.MUST_NOT)
      builder.build()
    }
  }

  case class And(criteria: SearchCriterion*) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      val builder = new BooleanQuery.Builder()
      criteria.foreach { criterion =>
        builder.add(criterion.toLuceneQuery(analyzerGroup), Occur.MUST)
      }
      builder.build()
    }
  }

  case class Or(criteria: SearchCriterion*) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      val builder = new BooleanQuery.Builder()
      criteria.foreach { criterion =>
        builder.add(criterion.toLuceneQuery(analyzerGroup), Occur.SHOULD)
      }
      builder.build()
    }
  }
}
