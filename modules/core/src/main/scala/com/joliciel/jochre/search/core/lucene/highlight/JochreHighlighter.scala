package com.joliciel.jochre.search.core.lucene.highlight

import com.joliciel.jochre.search.core.IndexField
import com.joliciel.jochre.search.core.lucene.{NEWLINE_TOKEN, PAGE_TOKEN, Token}
import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, OffsetAttribute}
import org.apache.lucene.queries.spans.{SpanNearQuery, SpanOrQuery, SpanQuery, SpanTermQuery}
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.highlight.{Highlighter, QueryScorer, SimpleHTMLFormatter, TextFragment}
import org.apache.lucene.search.{BooleanQuery, MatchNoDocsQuery, PhraseQuery, Query, TermQuery}
import org.apache.lucene.util.PriorityQueue
import org.slf4j.LoggerFactory

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._

case class JochreHighlighter(query: Query, field: IndexField) {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.search.highlighter")
  protected val formatter =
    new SimpleHTMLFormatter(config.getString("formatter-pre-tag"), config.getString("formatter-post-tag"))

  private val highlightQuery = toSpanQuery(query).getOrElse(new MatchNoDocsQuery())

  // Highlighter only works correctly with span queries, especially in the case of PhraseQuery with wildcards
  private def toSpanQuery(query: Query): Option[SpanQuery] = query match {
    case query: SpanQuery => Some(query)
    case query: PhraseQuery =>
      Option.when(query.getField == field.entryName) {
        val builder = new SpanNearQuery.Builder(field.entryName, true)
        builder.setSlop(query.getSlop)
        val termsAndPositions = query.getTerms.zip(query.getPositions)
        termsAndPositions.foldLeft(0) { case (currentPos, (term, position)) =>
          if (position > currentPos) {
            builder.addGap(position - currentPos)
          }
          builder.addClause(new SpanTermQuery(term))
          position + 1
        }
        builder.build()
      }
    case query: TermQuery =>
      Option.when(query.getTerm.field() == field.entryName) {
        new SpanTermQuery(query.getTerm)
      }
    case query: BooleanQuery =>
      val allClauses = query.clauses().asScala
      val positiveClauses = allClauses
        .filter(clause => clause.getOccur == Occur.SHOULD || clause.getOccur == Occur.MUST)
        .flatMap(clause => toSpanQuery(clause.getQuery))

      Option.when(positiveClauses.nonEmpty) {
        if (positiveClauses.length == 1) {
          positiveClauses.head
        } else {
          new SpanOrQuery(positiveClauses.toArray: _*)
        }
      }
    case other =>
      log.info(f"Cannot convert $other to span query")
      None
  }

  val scorer = {
    val scorer = new QueryScorer(highlightQuery, field.entryName)
    scorer.setExpandMultiTermQuery(true)
    scorer
  }

  private val highlighter: Highlighter = {
    val highlighter: Highlighter = new Highlighter(formatter, scorer)
    highlighter.setMaxDocCharsToAnalyze(Int.MaxValue)
    highlighter.setTextFragmenter(new RowFragmenter())
    highlighter
  }

  def findPreformattedSnippets(
      tokenStream: TokenStream,
      text: String,
      maxSnippets: Int
  ): Seq[PreformattedHighlightFragment] = {
    val mergeContiguousFragments = false

    // wrap to avoid array copy warning
    ArraySeq
      .unsafeWrapArray {
        highlighter.getBestTextFragments(tokenStream, text, mergeContiguousFragments, maxSnippets)
      }
      .filter(_.getScore > 0)
      .map(PreformattedHighlightFragment(_))
  }

  def findTerms(tokenStream: TokenStream): Seq[Token] = {
    val scoredStream = Option(scorer.init(tokenStream)).getOrElse(tokenStream)
    val termAtt = scoredStream.addAttribute(classOf[CharTermAttribute])
    val offsetAtt = scoredStream.addAttribute(classOf[OffsetAttribute])
    val dummyFragment = new TextFragment("", 0, 0)
    scorer.startFragment(dummyFragment)
    scoredStream.reset()

    List
      .unfold(scoredStream.incrementToken()) {
        case true =>
          val tokenScore = scorer.getTokenScore
          Some(
            Option.when(tokenScore > 0.0)(
              Token(termAtt.toString, offsetAtt.startOffset(), offsetAtt.endOffset(), tokenScore)
            )
              -> scoredStream.incrementToken()
          )
        case false => None
      }
      .flatten
  }

  /** @param tokenStream
    *   the token stream to analyze
    * @param maxSnippets
    *   the maximum number of snippets to return
    * @param rowPadding
    *   how many rows without highlights to add before and after each row containing highlights
    */
  def findSnippets(tokenStream: TokenStream, maxSnippets: Int, rowPadding: Int): Seq[HighlightFragment] = {
    val scoredStream = Option(scorer.init(tokenStream)).getOrElse(tokenStream)
    val termAtt = scoredStream.addAttribute(classOf[CharTermAttribute])
    val offsetAtt = scoredStream.addAttribute(classOf[OffsetAttribute])
    val dummyFragment = new TextFragment("", 0, 0)
    scorer.startFragment(dummyFragment)
    scoredStream.reset()

    case class OpenFragment(startOffset: Int) {
      var tokens: Vector[Token] = Vector.empty
      var rowTokenCounts: Vector[Int] = Vector.empty
      var lastRowStartOffset: Int = startOffset

      def newRow(offset: Int): Unit = {
        // If the last token overlaps this row, we start with a count of 1
        val initialCount = if (tokens.nonEmpty && tokens.last.end > offset) {
          1
        } else {
          0
        }
        rowTokenCounts = rowTokenCounts :+ initialCount
        lastRowStartOffset = offset
      }

      def addToken(token: Token): Unit = {
        tokens = tokens :+ token
        rowTokenCounts = rowTokenCounts.updated(rowTokenCounts.size - 1, rowTokenCounts.last + 1)
      }

      /** It is assumed the latest row was added just before calling shouldClose (so that we can check if a hyphenated
        * token overlaps this row).
        */
      def shouldClose(): Boolean = {
        // If before adding the current row, we had not yet reached the padding, don't close
        if (rowTokenCounts.size - 1 <= rowPadding) return false
        // If before adding the current row, we had already gone beyond the padding, and there are no tokens, close
        if (rowTokenCounts.size - 1 > rowPadding && tokens.isEmpty) return true
        // If a token overlaps the row that was just added (split hyphenated token) we don't close
        if (tokens.last.end > lastRowStartOffset) return false

        // We are now guaranteed that there is at least 1 token, check if it followed by n empty rows
        // We add 1 to the row padding, because the last row just got added, and is guaranteed to be empty
        val endPaddingFinished = rowTokenCounts.takeRight(rowPadding + 1).forall(_ == 0)
        endPaddingFinished
      }
    }

    var openFragments: Seq[OpenFragment] = Vector.empty
    class HighlightFragmentQueue(maxSnippets: Int) extends PriorityQueue[HighlightFragment](maxSnippets) {
      override def lessThan(a: HighlightFragment, b: HighlightFragment): Boolean = {
        if (a.score == b.score) {
          // if score is equal, we assume the longer spanning fragment is "greater"
          if (a.start == b.start) {
            a.end < b.end
          } else {
            a.start > b.start
          }
        } else {
          a.score < b.score
        }
      }
    }

    val fragmentQueue = new HighlightFragmentQueue(maxSnippets)

    var page = 0

    Iterator
      .continually(scoredStream.incrementToken())
      .takeWhile { hasNext =>
        // take while there's a next token
        hasNext
      }
      .foreach { _ =>
        val term = new String(termAtt.buffer().slice(0, termAtt.length()))
        val pageStart = term == PAGE_TOKEN
        if (pageStart) {
          val pageStartOffset = offsetAtt.startOffset()
          if (log.isTraceEnabled) {
            log.trace(f"Page start at offset $pageStartOffset, closing ${openFragments.size} fragments.")
          }
          // close all open fragments
          openFragments.foreach { closedFragment =>
            if (closedFragment.tokens.size > 0) {
              val highlightFragment =
                HighlightFragment(closedFragment.startOffset, pageStartOffset, page, closedFragment.tokens)

              if (log.isTraceEnabled) {
                log.trace(f"Created ${highlightFragment}")
              }
              fragmentQueue.insertWithOverflow(highlightFragment)
            }
          }
          openFragments = Vector.empty
          page = page + 1
        }
        val rowStart = term == NEWLINE_TOKEN
        if (rowStart) {
          val rowStartOffset = offsetAtt.startOffset()

          val newOpenFragment = OpenFragment(offsetAtt.startOffset())
          openFragments = openFragments :+ newOpenFragment
          openFragments.foreach(_.newRow(rowStartOffset))

          val (closedFragments, stillOpen) = openFragments.span(_.shouldClose())

          if (log.isTraceEnabled) {
            log.trace(
              f"Row start at offset $rowStartOffset. After new row, ${closedFragments.size} closed fragments, ${stillOpen.size} open."
            )
          }

          closedFragments.foreach { closedFragment =>
            if (closedFragment.tokens.size > 0) {
              val highlightFragment =
                HighlightFragment(closedFragment.startOffset, rowStartOffset, page, closedFragment.tokens)
              if (log.isTraceEnabled) {
                log.trace(f"Created $highlightFragment")
              }
              fragmentQueue.insertWithOverflow(highlightFragment)
            }
          }
          openFragments = stillOpen
        }
        val tokenScore = scorer.getTokenScore
        if (tokenScore > 0.0) {
          val token = Token(termAtt.toString, offsetAtt.startOffset(), offsetAtt.endOffset(), tokenScore)
          if (log.isTraceEnabled) {
            log.trace(f"Found token $token")
          }
          openFragments.foreach(_.addToken(token))
        }
      }

    if (log.isTraceEnabled) {
      log.trace(f"At end of token stream, closing ${openFragments.size} fragments.")
    }

    openFragments.foreach { stillOpen =>
      if (stillOpen.tokens.size > 0) {
        val highlightFragment =
          HighlightFragment(stillOpen.startOffset, offsetAtt.endOffset(), page, stillOpen.tokens)
        if (log.isTraceEnabled) {
          log.trace(f"Created $highlightFragment")
        }
        fragmentQueue.insertWithOverflow(highlightFragment)
      }
    }

    val bestFragments = (1 to fragmentQueue.size()).map(_ => fragmentQueue.pop()).sortBy(_.start)

    // Merge overlapping or contiguous fragments
    def mergeOverlappingFragments(fragments: Seq[HighlightFragment]): Seq[HighlightFragment] =
      fragments match {
        case first +: second +: tail =>
          if (first.end >= second.start && first.page == second.page) {
            val mergedTokens = (first.tokens ++ second.tokens).toSet.toSeq
            val sortedTokens = mergedTokens.sortBy(_.start)

            val mergedFragment = HighlightFragment(first.start, second.end, first.page, sortedTokens)
            if (log.isTraceEnabled) {
              log.trace(f"Merged $mergedFragment from $first and $second")
            }
            val newFragments = mergedFragment +: tail
            mergeOverlappingFragments(newFragments)
          } else {
            first +: mergeOverlappingFragments(second +: tail)
          }
        case other =>
          other
      }

    val mergedFragments = mergeOverlappingFragments(bestFragments)
    mergedFragments
  }
}
