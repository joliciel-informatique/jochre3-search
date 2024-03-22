package com.joliciel.jochre.search.core.lucene.highlight

import com.joliciel.jochre.search.core.lucene.{LuceneField, Token}
import com.joliciel.jochre.search.core.search.Snippet
import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, OffsetAttribute}
import org.apache.lucene.search.Query
import org.apache.lucene.search.highlight.{Highlighter, QueryScorer, SimpleHTMLFormatter, TextFragment}

import scala.collection.immutable.ArraySeq

case class JochreHighlighter(query: Query, field: LuceneField) {
  private val config = ConfigFactory.load().getConfig("jochre.search.highlighter")
  protected val formatter =
    new SimpleHTMLFormatter(config.getString("formatter-pre-tag"), config.getString("formatter-post-tag"))
  val scorer = new QueryScorer(query, field.name)

  private val highlighter: Highlighter = {
    val highlighter: Highlighter = new Highlighter(formatter, scorer)
    highlighter.setMaxDocCharsToAnalyze(Int.MaxValue)
    highlighter.setTextFragmenter(new RowFragmenter())
    highlighter
  }

  def findSnippets(tokenStream: TokenStream, text: String, maxSnippets: Int): Seq[HighlightFragment] = {
    val mergeContiguousFragments = false

    // wrap to avoid array copy warning
    ArraySeq
      .unsafeWrapArray {
        highlighter.getBestTextFragments(tokenStream, text, mergeContiguousFragments, maxSnippets)
      }
      .filter(_.getScore > 0)
      .map(HighlightFragment(_))
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
}
