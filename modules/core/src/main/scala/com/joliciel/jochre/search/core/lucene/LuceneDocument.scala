package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.highlight.{HighlightFragment, JochreHighlighter}
import com.joliciel.jochre.search.core.search.{DocRev, Highlight, Snippet}
import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.document.Document
import org.apache.lucene.index.Fields
import org.apache.lucene.search.Query
import org.apache.lucene.search.highlight.TokenSources

import scala.util.Using

private[lucene] class LuceneDocument(protected val indexSearcher: JochreSearcher, val luceneId: Int) {
  private val config = ConfigFactory.load().getConfig("jochre.search.highlighter")
  private val highlightPreTag = config.getString("formatter-pre-tag")
  private val highlightPostTag = config.getString("formatter-post-tag")
  private val defaultMaxSnippetCount = config.getInt("default-max-snippets")
  private val defaultRowPadding = config.getInt("default-row-padding")

  lazy val doc: Document = indexSearcher.storedFields.document(luceneId)
  lazy val ref: DocReference = DocReference(doc.get(LuceneField.Reference.entryName))
  lazy val rev: DocRev = DocRev(doc.get(LuceneField.Revision.entryName).toLong)
  lazy val termVector: Option[Fields] = Option(indexSearcher.getIndexReader.getTermVectors(luceneId))

  private def getTokenStream(field: LuceneField): Option[TokenStream] = {
    val maxStartOffset = -1
    termVector.flatMap { termVector =>
      Option(TokenSources.getTermVectorTokenStreamOrNull(field.entryName, termVector, maxStartOffset))
    }
  }

  def getText(field: LuceneField): Option[String] = Option(doc.get(field.entryName))

  private def getTokenStreamAndText(field: LuceneField): (Option[TokenStream], Option[String]) = {
    this.getTokenStream(field) -> this.getText(field);
  }

  def highlight(query: Query, maxSnippets: Option[Int] = None, rowPadding: Option[Int] = None): Seq[Snippet] = {
    val effectiveMaxSnippets = maxSnippets.getOrElse(defaultMaxSnippetCount)
    val effectiveRowPadding = rowPadding.getOrElse(defaultRowPadding)
    val highlighter = JochreHighlighter(query, LuceneField.Text)

    this.getTokenStreamAndText(LuceneField.Text) match {
      case (Some(tokenStream), Some(text)) =>
        Using(tokenStream) { tokenStream =>
          val fragments = highlighter.findSnippets(tokenStream, effectiveMaxSnippets, effectiveRowPadding)
          fragments.map { case HighlightFragment(start, end, _, tokens) =>
            val snippetText = text.substring(start, end)
            val (sb, lastOffset) = tokens.foldLeft(new StringBuilder() -> start) { case ((sb, lastOffset), token) =>
              if (lastOffset < token.start) {
                sb.append(snippetText.substring(lastOffset - start, token.start - start))
              }
              sb.append(highlightPreTag)
              val tokenText = snippetText.substring(token.start - start, token.end - start)
              val tokenTextWithHighlights = tokenText.split("\n").mkString(f"$highlightPostTag\n$highlightPreTag")
              sb.append(tokenTextWithHighlights)
              sb.append(highlightPostTag)
              sb -> token.end
            }
            if (lastOffset < end) {
              sb.append(snippetText.substring(lastOffset - start))
            }
            val highlightedText = sb.toString().trim.replace("\n", "<br>")
            val highlights = tokens.map(token => Highlight(token.start, token.end))
            Snippet(highlightedText, start, end, highlights)
          }
        }.get
      case _ =>
        Seq.empty
    }
  }
}
