package com.joliciel.jochre.search.core.search

import com.joliciel.jochre.ocr.core.graphics.{Line, Rectangle}
import com.joliciel.jochre.ocr.core.model.{
  Alto,
  Hyphen,
  Page,
  Space,
  SpellingAlternative,
  TextBlock,
  TextLine,
  Word,
  WordOrSpace
}

trait AltoHelper {
  def textToAlto(fileName: String, text: String, alternativeMap: Map[String, Seq[String]]): Alto = {
    val pagesOfText = text.split("\n\n")

    val pages = pagesOfText.zipWithIndex.map { case (pageOfText, i) =>
      val linesOfText = pageOfText.split("\n")
      val lines = linesOfText.zipWithIndex.map { case (lineOfText, lineIndex) =>
        val wordClusters = lineOfText.split(raw"((?<= )|(?= ))")
        val (finalCharIndex, wordsAndSpaces) = wordClusters.foldLeft(0 -> Seq.empty[WordOrSpace]) {
          case ((charIndex, wordsAndSpaces), wordOrSpace) =>
            if (wordOrSpace == " ") {
              val space = Space(Rectangle(charIndex * 10, lineIndex * 10, 10, 10))
              (charIndex + 1) -> (wordsAndSpaces :+ space)
            } else {
              val wordParts = wordOrSpace.split(raw"((?<=\p{Punct})|(?=\p{Punct}))")
              val (finalCharIndex, words) = wordParts.foldLeft(charIndex -> Seq.empty[Word]) {
                case ((charIndex, words), wordText) =>
                  val alternatives = alternativeMap
                    .getOrElse(wordText.toLowerCase, Seq.empty)
                    .map(altText => SpellingAlternative("Synonym", altText))
                  val word = Word(
                    wordText,
                    Rectangle(charIndex * 10, lineIndex * 10, wordText.length * 10, 10),
                    glyphs = Seq.empty,
                    alternatives = alternatives,
                    confidence = 1.0
                  )
                  (charIndex + wordText.length) -> (words :+ word)
              }
              finalCharIndex -> (wordsAndSpaces ++ words)
            }
        }
        val wordsAndSpacesWithHyphen = if (wordsAndSpaces.lastOption.map(_.content) == Some("-")) {
          val lastWord = wordsAndSpaces.last
          wordsAndSpaces.init :+ Hyphen(lastWord.content, lastWord.rectangle)
        } else {
          wordsAndSpaces
        }
        TextLine(Line(0, lineIndex * 10 + 8, finalCharIndex * 10, lineIndex * 10 + 8), wordsAndSpacesWithHyphen)
      }
      val width = lines.map(_.baseLine.x2).maxOption.getOrElse(100)
      Page(
        id = f"Page$i",
        height = lines.length * 10,
        width = width,
        physicalPageNumber = i,
        rotation = 0.0,
        language = "en",
        confidence = 1.0,
        blocks = Seq(
          TextBlock(Rectangle(0, 0, width, lines.length * 10), textLines = lines, id = f"TextBlock$i")
        )
      )
    }

    Alto(fileName, pages = pages)
  }
}
