package com.joliciel.jochre.search.core.lucene

import java.io.StringReader
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.tokenattributes.{
  CharTermAttribute,
  OffsetAttribute,
  PayloadAttribute,
  PositionIncrementAttribute
}

private[lucene] trait LuceneUtilities {
  def asStemmedSet(words: Iterable[String], analyzer: Analyzer): Set[String] =
    tokenizeString(words.mkString(" "), analyzer)
      .collect { case token @ Token(_, _, _, _) =>
        token
      }
      .map(_.value)
      .toSet

  def asTokenizedSet(words: Iterable[String], analyzer: Analyzer): Set[String] =
    tokenizeString(words.mkString(" "), analyzer)
      .map(_.value)
      .toSet

  def asTokenizedString(string: String, analyzer: Analyzer): String = {
    val tokens = tokenizeString(string, analyzer)
    tokens.map(_.value).mkString(" ")
  }

  def tokenizeString(string: String, analyzer: Analyzer): Seq[Token] = {
    val tokenStream = analyzer.tokenStream(null, new StringReader(string))
    val termAtt = tokenStream.addAttribute(classOf[CharTermAttribute])
    val offsetAtt = tokenStream.addAttribute(classOf[OffsetAttribute])

    try {
      tokenStream.reset // required

      Iterator
        .continually(tokenStream.incrementToken())
        .takeWhile(hasNext => hasNext)
        .map(_ => Token(termAtt.toString, offsetAtt.startOffset(), offsetAtt.endOffset(), 1.0f))
        .toSeq
    } finally {
      tokenStream.close()
    }
  }

  def tokenizeStringWithPositions(string: String, analyzer: Analyzer): Seq[(Token, Int)] = {
    val tokenStream = analyzer.tokenStream(null, new StringReader(string))
    val termAtt = tokenStream.addAttribute(classOf[CharTermAttribute])
    val offsetAtt = tokenStream.addAttribute(classOf[OffsetAttribute])
    val positionIncrementAtt = tokenStream.addAttribute(classOf[PositionIncrementAttribute])

    try {
      tokenStream.reset // required

      Iterator
        .continually(tokenStream.incrementToken())
        .takeWhile(hasNext => hasNext)
        .map(_ =>
          Token(
            termAtt.toString,
            offsetAtt.startOffset(),
            offsetAtt.endOffset(),
            1.0f
          ) -> positionIncrementAtt.getPositionIncrement
        )
        .toSeq
    } finally {
      tokenStream.close()
    }
  }

  def tokenizeStringWithPositionsAndPayloads(string: String, analyzer: Analyzer): Seq[(Token, Int, JochrePayload)] = {
    val tokenStream = analyzer.tokenStream(null, new StringReader(string))
    val termAtt = tokenStream.addAttribute(classOf[CharTermAttribute])
    val offsetAtt = tokenStream.addAttribute(classOf[OffsetAttribute])
    val positionIncrementAtt = tokenStream.addAttribute(classOf[PositionIncrementAttribute])
    val payloadAtt = tokenStream.addAttribute(classOf[PayloadAttribute])

    try {
      tokenStream.reset // required

      Iterator
        .continually(tokenStream.incrementToken())
        .takeWhile(hasNext => hasNext)
        .map(_ =>
          (
            Token(termAtt.toString, offsetAtt.startOffset(), offsetAtt.endOffset(), 1.0f),
            positionIncrementAtt.getPositionIncrement,
            JochrePayload(payloadAtt.getPayload)
          )
        )
        .toSeq
    } finally {
      tokenStream.close()
    }
  }
}
