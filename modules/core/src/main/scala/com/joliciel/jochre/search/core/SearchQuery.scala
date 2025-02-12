package com.joliciel.jochre.search.core

import com.joliciel.jochre.search.core.lucene.{AnalyzerGroup, JochreMultiFieldQueryParser}
import org.apache.lucene.document.IntPoint
import org.apache.lucene.index.Term
import org.apache.lucene.queryparser.classic.ParseException
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.{BooleanQuery, MatchAllDocsQuery, PrefixQuery, Query, TermInSetQuery}
import org.apache.lucene.util.BytesRef

import scala.jdk.CollectionConverters._
import com.joliciel.jochre.search.core.lucene.LuceneUtilities

case class SearchQuery(criterion: SearchCriterion) {
  def replaceQuery(replaceFunction: String => String): SearchQuery = {
    this.copy(criterion.replaceCriterion(replaceFunction))
  }
}

sealed trait SearchCriterion {
  private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query
  private[core] def getContains(): Option[SearchCriterion.Contains] = None
  def replaceCriterion(replaceFunction: String => String): SearchCriterion = this
}

object SearchCriterion extends LuceneUtilities {
  case object MatchAllDocuments extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = new MatchAllDocsQuery()
  }

  case class Contains(fields: Seq[IndexField], queryString: String, strict: Boolean) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      fields.foreach { field =>
        if (!field.isTokenized) {
          throw new WrongFieldTypeException(
            f"Cannot perform Contains on field ${field.fieldName} - field not tokenized"
          )
        }
      }
      val fixedCriterion = analyzerGroup.preformatCriterion(this)
      val fixedQueryString = fixedCriterion match {
        case Contains(_, fixedQueryString, _) => fixedQueryString
        case _                                => queryString
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
      try {
        parser.parse(fixedQueryString)
      } catch {
        case pe: ParseException =>
          throw new UnparsableQueryException(pe.getMessage)
      }
    }

    override private[core] def getContains(): Option[Contains] = Some(this)

    override def replaceCriterion(replaceFunction: String => String): Contains =
      Contains(fields, replaceFunction(queryString), strict)
  }

  object Contains {
    def apply(field: IndexField, queryString: String, strict: Boolean = false): Contains =
      Contains(Seq(field), queryString, strict)
  }

  case class ValueIn(field: IndexField, values: Seq[String]) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      if (field.isTokenized) {
        throw new WrongFieldTypeException(f"Cannot perform ValueIn on field ${field.fieldName}: field is tokenized")
      }
      val normalizedValues = values.map { str =>
        asTokenizedString(str, analyzerGroup.forIndexingUntokenizedFields)
      }
      new TermInSetQuery(
        field.fieldName,
        normalizedValues
          .map(str => new BytesRef(str))
          .asJavaCollection
      )
    }
  }

  case class StartsWith(field: IndexField, prefix: String) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      if (field.isTokenized) {
        throw new WrongFieldTypeException(f"Cannot perform StartsWith on field ${field.fieldName}: field is tokenized")
      }
      val normalizedPrefix = asTokenizedString(prefix, analyzerGroup.forIndexingUntokenizedFields)
      val prefixQuery = new PrefixQuery(
        new Term(
          field.fieldName,
          normalizedPrefix
        )
      )
      prefixQuery
    }
  }

  case class GreaterThanOrEqualTo(field: IndexField, value: Int) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      if (field.kind != FieldKind.Integer) {
        throw new WrongFieldTypeException(f"Field ${field.fieldName} is not an integer, cannot do GreaterThanOrEqualTo")
      }
      IntPoint.newRangeQuery(field.fieldName, value, Int.MaxValue)
    }
  }

  case class LessThanOrEqualTo(field: IndexField, value: Int) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      if (field.kind != FieldKind.Integer) {
        throw new WrongFieldTypeException(f"Field ${field.fieldName} is not an integer, cannot do LessThanOrEqualTo")
      }
      IntPoint.newRangeQuery(field.fieldName, Int.MinValue, value)
    }
  }

  case class Not(criterion: SearchCriterion) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      val builder = new BooleanQuery.Builder()
      builder.add(new MatchAllDocsQuery(), Occur.SHOULD)
      builder.add(criterion.toLuceneQuery(analyzerGroup), Occur.MUST_NOT)
      builder.build()
    }

    override private[core] def getContains(): Option[Contains] = criterion.getContains()

    override def replaceCriterion(replaceFunction: String => String): Not = Not(
      criterion.replaceCriterion(replaceFunction)
    )
  }

  case class And(criteria: SearchCriterion*) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      val builder = new BooleanQuery.Builder()
      criteria.foreach { criterion =>
        builder.add(criterion.toLuceneQuery(analyzerGroup), Occur.MUST)
      }
      builder.build()
    }

    override private[core] def getContains(): Option[Contains] = criteria.map(_.getContains()).flatten.headOption

    override def replaceCriterion(replaceFunction: String => String): And = And(
      criteria.map(_.replaceCriterion(replaceFunction))*
    )
  }

  case class Or(criteria: SearchCriterion*) extends SearchCriterion {
    override private[core] def toLuceneQuery(analyzerGroup: AnalyzerGroup): Query = {
      val builder = new BooleanQuery.Builder()
      criteria.foreach { criterion =>
        builder.add(criterion.toLuceneQuery(analyzerGroup), Occur.SHOULD)
      }
      builder.build()
    }

    override private[core] def getContains(): Option[Contains] = criteria.map(_.getContains()).flatten.headOption

    override def replaceCriterion(replaceFunction: String => String): Or = Or(
      criteria.map(_.replaceCriterion(replaceFunction))*
    )
  }
}
