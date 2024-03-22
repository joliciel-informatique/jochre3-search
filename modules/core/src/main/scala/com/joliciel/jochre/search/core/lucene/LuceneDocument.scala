package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.highlight.{HighlightFragment, JochreHighlighter}
import com.joliciel.jochre.search.core.search.{Highlight, Snippet}
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, OffsetAttribute}
import org.apache.lucene.document.Document
import org.apache.lucene.index.Fields
import org.apache.lucene.search.Query
import org.apache.lucene.search.highlight.{TextFragment, TokenSources}

import scala.collection.immutable.SortedMap
import scala.util.Using

private[lucene] class LuceneDocument(protected val indexSearcher: JochreSearcher, val luceneId: Int) {
  lazy val doc: Document = indexSearcher.storedFields.document(luceneId)
  lazy val ref: DocReference = DocReference(doc.get(LuceneField.Id.name))
  lazy val termVector: Option[Fields] = Option(indexSearcher.getIndexReader.getTermVectors(luceneId))

  private def getTokenStream(field: LuceneField): Option[TokenStream] = {
    val maxStartOffset = -1
    termVector.flatMap { termVector =>
      Option(TokenSources.getTermVectorTokenStreamOrNull(field.name, termVector, maxStartOffset))
    }
  }

  def getText(field: LuceneField): Option[String] = Option(doc.get(field.name))

  private def getTokenStreamAndText(field: LuceneField): (Option[TokenStream], Option[String]) = {
    this.getTokenStream(field) -> this.getText(field);
  }

  val defaultMaxSnippetCount = 20

  def findSnippetsAsText(query: Query, maxSnippets: Option[Int] = None): Seq[String] = {
    val effectiveMaxSnippets = maxSnippets.getOrElse(defaultMaxSnippetCount)

    val fragments = this.findHighlightFragments(query, effectiveMaxSnippets)

    fragments.sorted
      .take(effectiveMaxSnippets)
      .map(_.text)
  }

  def highlight(query: Query, maxSnippets: Option[Int] = None): Seq[Snippet] = {
    val effectiveMaxSnippets = maxSnippets.getOrElse(defaultMaxSnippetCount)
    val terms = this
      .findTerms(query)
      .map { term => term.start -> term }
    val termMap = SortedMap.empty[Int, Token] ++ terms

    val highlightFragments = this.findHighlightFragments(query, effectiveMaxSnippets)
    highlightFragments.map { fragment =>
      val myTerms = termMap.rangeImpl(Some(fragment.start), Some(fragment.end))
      val highlights = myTerms.view.values.map(token => Highlight(token.start, token.end)).toSeq
      // Drop (1) from fragment for the initial newline
      Snippet(fragment.text.drop(1), fragment.start, fragment.end, 0, 0, 0, highlights)
    }
  }

  private def findHighlightFragments(query: Query, maxSnippets: Int): Seq[HighlightFragment] = {
    this.getTokenStreamAndText(LuceneField.Text) match {
      case (Some(tokenStream), Some(text)) =>
        Using(tokenStream) { tokenStream =>
          val highlighter = JochreHighlighter(query, LuceneField.Text)
          highlighter.findSnippets(tokenStream, text, maxSnippets)
        }.get
      case _ =>
        Seq.empty
    }
  }

  private def findTerms(query: Query): Seq[Token] = {
    this
      .getTokenStream(LuceneField.Text)
      .map { case tokenStream =>
        Using(tokenStream) { tokenStream =>
          val highlighter = JochreHighlighter(query, LuceneField.Text)
          highlighter.findTerms(tokenStream)
        }.get
      }
      .getOrElse(Seq.empty)
  }
}
