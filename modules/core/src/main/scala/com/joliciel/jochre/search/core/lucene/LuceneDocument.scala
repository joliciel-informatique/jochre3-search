package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.{DocMetadata, DocReference, IndexField, MetadataField}
import com.joliciel.jochre.search.core.lucene.highlight.{HighlightFragment, JochreHighlighter}
import com.joliciel.jochre.search.core.service.{DocRev, Highlight, HighlightedPage, Snippet}
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
  lazy val ref: DocReference = DocReference(doc.get(IndexField.Reference.entryName))
  lazy val rev: DocRev = DocRev(doc.get(IndexField.Revision.entryName).toLong)
  lazy val termVector: Option[Fields] = Option {
    val termVectors = indexSearcher.getIndexReader.termVectors()
    termVectors.get(luceneId)
  }

  lazy val metadata: DocMetadata =
    DocMetadata(
      title = Option(doc.get(IndexField.Title.entryName)),
      titleEnglish = Option(doc.get(IndexField.TitleEnglish.entryName)),
      author = Option(doc.get(IndexField.Author.entryName)),
      authorEnglish = Option(doc.get(IndexField.AuthorEnglish.entryName)),
      publisher = Option(doc.get(IndexField.Publisher.entryName)),
      publicationYear = Option(doc.get(IndexField.PublicationYear.entryName)),
      volume = Option(doc.get(IndexField.Volume.entryName)),
      url = Option(doc.get(IndexField.URL.entryName)),
      collections = doc.getValues(IndexField.Collection.entryName)
    )

  lazy val ocrSoftware: Option[String] = Option(doc.get(IndexField.OCRSoftware.entryName))

  private def getTokenStream(field: IndexField): Option[TokenStream] = {
    val maxStartOffset = -1
    termVector.flatMap { termVector =>
      Option(TokenSources.getTermVectorTokenStreamOrNull(field.entryName, termVector, maxStartOffset))
    }
  }

  def getMetaValue(field: MetadataField): Option[String] = Option(doc.get(field.indexField.entryName))

  def getText(field: IndexField): Option[String] = Option(doc.get(field.entryName))

  private def getTokenStreamAndText(field: IndexField): (Option[TokenStream], Option[String]) = {
    this.getTokenStream(field) -> this.getText(field);
  }

  def highlight(
      query: Query,
      maxSnippets: Option[Int] = None,
      rowPadding: Option[Int] = None,
      addOffsets: Boolean = true
  ): Seq[Snippet] = {
    val effectiveMaxSnippets = maxSnippets.getOrElse(defaultMaxSnippetCount)
    val effectiveRowPadding = rowPadding.getOrElse(defaultRowPadding)
    val highlighter = JochreHighlighter(query, IndexField.Text)

    this.getTokenStreamAndText(IndexField.Text) match {
      case (Some(tokenStream), Some(text)) =>
        Using(tokenStream) { tokenStream =>
          val fragments = highlighter.findSnippets(tokenStream, effectiveMaxSnippets, effectiveRowPadding)
          fragments.map { case HighlightFragment(start, end, page, tokens) =>
            val snippetText = text.substring(start, end)
            val snippetLines = snippetText.split("\n")
            val (_, lineFragments) = snippetLines.foldLeft(start -> Seq.empty[HighlightFragment]) {
              case ((prevOffset, lineFragments), snippetLine) =>
                val lineStart = prevOffset
                val lineEnd = prevOffset + snippetLine.length
                val myTokens = tokens.filter(token => token.end >= lineStart && token.start < lineEnd)
                val fragment = HighlightFragment(lineStart, lineEnd, page, myTokens)
                // Add 1 to the line end for the newline character
                (lineEnd + 1) -> (lineFragments :+ fragment)
            }

            val lines =
              lineFragments.zip(snippetLines).map { case (HighlightFragment(start, end, _, tokens), snippetLine) =>
                val sb = new StringBuilder()
                val lastOffset = tokens.foldLeft(start) { case (lastOffset, token) =>
                  if (lastOffset < token.start) {
                    if (addOffsets) {
                      sb.append("<span offset=\"" + lastOffset + "\">")
                    }
                    sb.append(snippetLine.substring(lastOffset - start, token.start - start))
                    if (addOffsets) {
                      sb.append("</span>")
                    }
                  }
                  val tokenStart = if (token.start < start) {
                    start
                  } else {
                    token.start
                  }
                  val tokenEnd = if (token.end > end) {
                    end
                  } else {
                    token.end
                  }
                  val tokenText = snippetLine.substring(tokenStart - start, tokenEnd - start)

                  sb.append(highlightPreTag)
                  if (addOffsets) {
                    sb.append("<span offset=\"" + tokenStart + "\">")
                  }
                  sb.append(tokenText)
                  if (addOffsets) {
                    sb.append("</span>")
                  }
                  sb.append(highlightPostTag)

                  tokenEnd
                }

                if (lastOffset < end) {
                  if (addOffsets) {
                    sb.append("<span offset=\"" + lastOffset + "\">")
                  }
                  sb.append(snippetLine.substring(lastOffset - start))
                  if (addOffsets) {
                    sb.append("</span>")
                  }
                }

                sb.toString()
              }

            val highlightedText = lines.mkString("<br>")
            val highlights = tokens.map(token => Highlight(token.start, token.end))
            Snippet(highlightedText, -1, start, end, highlights, deepLink = None)
          }
        }.get
      case _ =>
        Seq.empty
    }
  }

  /** Returns, for each page, the start offset and the highlighted text.
    * @param query
    *   the query to use for highlighting
    */
  def highlightPagesAsHtml(
      query: Query
  ): Seq[(Int, String)] = {
    val highlighter = JochreHighlighter(query, IndexField.Text)

    this.getTokenStreamAndText(IndexField.Text) match {
      case (Some(tokenStream), Some(text)) =>
        Using.resource(tokenStream) { tokenStream =>
          val terms = highlighter.findTerms(tokenStream, includePageBreaks = true)
          val (pageBuilders, lastPos) = terms.foldLeft(Vector(0 -> new StringBuilder()) -> 0) {
            case ((pageBuilders, lastPos), token) =>
              val leftover = text.substring(lastPos, token.start).replaceAll("\n", "<br>")
              val (_, sb) = pageBuilders.last
              if (token.value == PAGE_TOKEN) {
                sb.append(leftover)
                (pageBuilders :+ (token.start, new StringBuilder()), token.start)
              } else {
                sb.append(leftover)
                val highlight = text.substring(token.start, token.end)
                sb.append(highlightPreTag)
                sb.append(highlight.replaceAll("\n", f"$highlightPostTag<br>$highlightPreTag"))
                sb.append(highlightPostTag)
                (pageBuilders, token.end)
              }
          }
          if (lastPos < text.length) {
            val (_, sb) = pageBuilders.last
            sb.append(text.substring(lastPos, text.length).replaceAll("\n", "<br>"))
          }
          // We take the tail to skip the "fake" page containing the document reference
          val pageTexts = pageBuilders.tail.map { case (startOffset, sb) => startOffset -> sb.toString() }
          pageTexts
        }
      case _ =>
        Seq.empty
    }
  }

  def highlightPages(
      query: Query,
      textAsHtml: Boolean
  ): Seq[HighlightedPage] = {
    val highlighter = JochreHighlighter(query, IndexField.Text)

    this.getTokenStreamAndText(IndexField.Text) match {
      case (Some(tokenStream), Some(text)) =>
        Using.resource(tokenStream) { tokenStream =>
          val terms = highlighter.findTerms(tokenStream, includePageBreaks = true)
          val (pageBuilders, lastPos) = terms.foldLeft(Vector((0, new StringBuilder(), Seq.empty[Highlight])) -> 0) {
            case ((pageBuilders, lastPos), token) =>
              val leftover = if (textAsHtml) {
                text.substring(lastPos, token.start).replaceAll("\n", "<br>")
              } else {
                text.substring(lastPos, token.start)
              }
              val (pageStart, sb, highlights) = pageBuilders.last
              if (token.value == PAGE_TOKEN) {
                sb.append(leftover)
                (pageBuilders :+ (token.start, new StringBuilder(), Seq.empty[Highlight]), token.start)
              } else {
                sb.append(leftover)
                val highlightText = text.substring(token.start, token.end)
                if (textAsHtml) {
                  sb.append(highlightPreTag)
                  sb.append(highlightText.replaceAll("\n", f"$highlightPostTag<br>$highlightPreTag"))
                  sb.append(highlightPostTag)
                } else {
                  sb.append(highlightText)
                }
                val highlight = Highlight(token.start - pageStart, token.end - pageStart)
                val newPageBuilders = pageBuilders.init :+ (pageStart, sb, highlights :+ highlight)
                (newPageBuilders, token.end)
              }
          }
          if (lastPos < text.length) {
            val (_, sb, _) = pageBuilders.last
            val leftover = if (textAsHtml) {
              text.substring(lastPos, text.length).replaceAll("\n", "<br>")
            } else {
              text.substring(lastPos, text.length)
            }
            sb.append(leftover)
          }
          // We take the tail to skip the "fake" page containing the document reference
          val pages = pageBuilders.tail.map { case (startOffset, sb, highlights) =>
            HighlightedPage(0, startOffset, sb.toString(), highlights)
          }
          pages
        }
      case _ =>
        Seq.empty
    }
  }
}
