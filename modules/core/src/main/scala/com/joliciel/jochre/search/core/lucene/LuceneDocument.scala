package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.DocMetadata
import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.IndexField
import com.joliciel.jochre.search.core.MetadataField
import com.joliciel.jochre.search.core.lucene.highlight.HighlightFragment
import com.joliciel.jochre.search.core.lucene.highlight.JochreHighlighter
import com.joliciel.jochre.search.core.service.DocRev
import com.joliciel.jochre.search.core.service.Highlight
import com.joliciel.jochre.search.core.service.HighlightedPage
import com.joliciel.jochre.search.core.service.Snippet
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.document.Document
import org.apache.lucene.index.Fields
import org.apache.lucene.search.Query
import org.apache.lucene.search.highlight.TokenSources

import scala.util.Using
import org.slf4j.LoggerFactory

private[lucene] class LuceneDocument(protected val indexSearcher: JochreSearcher, val luceneId: Int) {
  private val log = LoggerFactory.getLogger(getClass)

  private val config = ConfigFactory.load().getConfig("jochre.search.highlighter")
  private val highlightPreTag = config.getString("formatter-pre-tag")
  private val highlightPostTag = config.getString("formatter-post-tag")
  private val defaultMaxSnippetCount = config.getInt("default-max-snippets")
  private val defaultRowPadding = config.getInt("default-row-padding")
  private val snippetClass = config.getString("snippet-class")

  lazy val doc: Document = indexSearcher.storedFields.document(luceneId)
  lazy val ref: DocReference = DocReference(doc.get(IndexField.Reference.fieldName))
  lazy val rev: DocRev = DocRev(doc.get(IndexField.Revision.fieldName).toLong)
  lazy val termVector: Option[Fields] = Option {
    val termVectors = indexSearcher.getIndexReader.termVectors()
    termVectors.get(luceneId)
  }

  lazy val metadata: DocMetadata =
    DocMetadata(
      title = Option(doc.get(IndexField.Title.fieldName)),
      titleEnglish = Option(doc.get(IndexField.TitleEnglish.fieldName)),
      author = Option(doc.get(IndexField.Author.fieldName)),
      authorEnglish = Option(doc.get(IndexField.AuthorEnglish.fieldName)),
      publisher = Option(doc.get(IndexField.Publisher.fieldName)),
      publicationYear = Option(doc.get(IndexField.PublicationYear.fieldName)),
      volume = Option(doc.get(IndexField.Volume.fieldName)),
      url = Option(doc.get(IndexField.URL.fieldName)),
      collections = doc.getValues(IndexField.Collection.fieldName)
    )

  lazy val ocrSoftware: Option[String] = Option(doc.get(IndexField.OCRSoftware.fieldName))

  private def getTokenStream(field: IndexField): Option[TokenStream] = {
    val maxStartOffset = -1
    termVector.flatMap { termVector =>
      Option(TokenSources.getTermVectorTokenStreamOrNull(field.fieldName, termVector, maxStartOffset))
    }
  }

  def getMetaValue(field: MetadataField): Option[String] = Option(doc.get(field.indexField.fieldName))

  def getText(field: IndexField): Option[String] = Option(doc.get(field.fieldName))

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

  private val numberRegex = raw"\d+".r
  def highlightPages(
      query: Query,
      textAsHtml: Boolean,
      filters: Option[LanguageSpecificFilters] = None,
      simplifyText: Boolean = false
  ): Seq[HighlightedPage] = {
    val highlighter = JochreHighlighter(query, IndexField.Text)

    this.getTokenStreamAndText(IndexField.Text) match {
      case (Some(tokenStream), Some(text)) =>
        Using.resource(tokenStream) { tokenStream =>
          val terms = highlighter.findTerms(tokenStream, includePageBreaks = true)
          val (pageBuilders, lastPos) = terms.foldLeft(Vector((0, new StringBuilder(), Seq.empty[Highlight])) -> 0) {
            case ((pageBuilders, lastPos), token) =>
              if (log.isTraceEnabled()) {
                log.trace(f"Token ${token.value} from ${token.start} to ${token.end}. LastPos: $lastPos")
              }
              if (lastPos <= token.start) {
                val initialLeftover = text.substring(lastPos, token.start)

                val simplifiedLeftover = if (simplifyText) {
                  filters.map { filters => filters.simplifyText(initialLeftover) }.getOrElse(initialLeftover)
                } else {
                  initialLeftover
                }

                val leftover = if (textAsHtml) {
                  simplifiedLeftover.replaceAll("\n", "<br>")
                } else {
                  simplifiedLeftover
                }

                val (pageStart, sb, highlights) = pageBuilders.last
                if (token.value == PAGE_TOKEN) {
                  sb.append(leftover)
                  (pageBuilders :+ (token.start, new StringBuilder(), Seq.empty[Highlight]), token.start)
                } else {
                  sb.append(leftover)
                  val initialHighlightText = text.substring(token.start, token.end)
                  val highlightText = if (simplifyText) {
                    filters
                      .map { filters => filters.simplifyText(initialHighlightText) }
                      .getOrElse(initialHighlightText)
                  } else {
                    initialHighlightText
                  }
                  if (textAsHtml) {
                    sb.append(highlightPreTag)
                    sb.append(highlightText.replaceAll("\n", f"$highlightPostTag<br>$highlightPreTag"))
                    sb.append(highlightPostTag)
                  } else {
                    sb.append(highlightText)
                  }

                  // If a highlight cross a newline, create a separate highlight for each split across the newline
                  val splitHighlightTexts = highlightText.split('\n')
                  val (_, splitHighlights) = splitHighlightTexts.foldLeft(token.start -> Seq.empty[Highlight]) {
                    case ((currentStart, splitHighlights), splitHighlightText) =>
                      val nextStart = currentStart + splitHighlightText.length + 1
                      nextStart -> (splitHighlights :+ Highlight(
                        currentStart - pageStart,
                        (currentStart + splitHighlightText.length) - pageStart
                      ))
                  }

                  val newPageBuilders = pageBuilders.init :+ (pageStart, sb, highlights ++ splitHighlights)
                  (newPageBuilders, token.end)
                }
              } else {
                // For some reason we got a highlight for a string we already highlighted
                // most likely the same string highlighted twice
                // we ignore it
                (pageBuilders, lastPos)
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
            val initialText = sb.toString()
            val paragraphRegex = if (textAsHtml) "<br>" else "\n"
            val paragraphs = initialText.split(paragraphRegex)

            val logicalPageNumber = if (paragraphs.length > 0 && numberRegex.matches(paragraphs.head)) {
              paragraphs.head.toIntOption
            } else if (paragraphs.length > 1 && numberRegex.matches(paragraphs(1))) {
              paragraphs(1).toIntOption
            } else if (paragraphs.length > 0 && numberRegex.matches(paragraphs.last)) {
              paragraphs.last.toIntOption
            } else {
              None
            }

            val pageText = if (textAsHtml) {
              val innerTextWithDivs = initialText.replaceAll("<br><br>", f"""</div><div class="$snippetClass">""")
              f"""<div class="$snippetClass">$innerTextWithDivs</div>"""
            } else {
              initialText
            }
            HighlightedPage(0, startOffset, pageText, highlights, logicalPageNumber)
          }
          pages
        }
      case _ =>
        Seq.empty
    }
  }
}
