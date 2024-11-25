package com.joliciel.jochre.search.core.service

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
import org.slf4j.LoggerFactory

trait AltoHelper {
  private val log = LoggerFactory.getLogger(getClass)

  def textToAlto(fileName: String, text: String, alternativeMap: Map[String, Seq[String]]): Alto = {
    val pagesOfText = text.split("\n\n\n")

    val pages = pagesOfText.zipWithIndex.map { case (pageOfText, i) =>
      val blocksOfText = pageOfText.split("\n\n")
      val (textBlocks, _) = blocksOfText.zipWithIndex.foldLeft(Seq.empty[TextBlock] -> 0) {
        case ((textBlockSeq, startHeight), (blockOfText, blockIndex)) =>
          val linesOfText = blockOfText.split("\n")
          val lines = linesOfText.zipWithIndex.map { case (lineOfText, lineIndex) =>
            val wordClusters = lineOfText.split(raw"((?<= )|(?= ))")
            val (finalCharIndex, wordsAndSpaces) = wordClusters.foldLeft(0 -> Seq.empty[WordOrSpace]) {
              case ((charIndex, wordsAndSpaces), wordOrSpace) =>
                if (wordOrSpace == " ") {
                  val space = Space(Rectangle(charIndex * 10, startHeight + lineIndex * 10, 10, 10))
                  (charIndex + 1) -> (wordsAndSpaces :+ space)
                } else {
                  val wordParts = wordOrSpace.split(raw"(?U)((?<=\p{Punct})|(?=\p{Punct}))")
                  val (finalCharIndex, words) = wordParts.foldLeft(charIndex -> Seq.empty[Word]) {
                    case ((charIndex, words), wordText) =>
                      val alternatives = alternativeMap
                        .getOrElse(wordText.toLowerCase, Seq.empty)
                        .map(altText => SpellingAlternative("Synonym", altText))
                      val word = Word(
                        wordText,
                        Rectangle(charIndex * 10, startHeight + lineIndex * 10, wordText.length * 10, 10),
                        glyphs = Seq.empty,
                        alternatives = alternatives,
                        confidence = 1.0
                      )
                      log.debug(s"Added word: ${word}")
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
            TextLine(
              Line(0, startHeight + lineIndex * 10 + 8, finalCharIndex * 10, startHeight + lineIndex * 10 + 8),
              wordsAndSpacesWithHyphen
            )
          }
          val width = lines.map(_.baseLine.x2).maxOption.getOrElse(100)
          val height = lines.length * 10
          (textBlockSeq :+ TextBlock(
            Rectangle(0, startHeight, width, height),
            textLines = lines,
            id = f"TextBlock${i}_$blockIndex"
          )) -> (startHeight + height)
      }
      val width = textBlocks.map(_.width).maxOption.getOrElse(100)
      val height = textBlocks.flatMap(_.textLines).length * 10
      Page(
        id = f"Page$i",
        height = height,
        width = width,
        physicalPageNumber = i,
        rotation = 0.0,
        language = "en",
        confidence = 1.0,
        blocks = textBlocks
      )
    }

    Alto(fileName, pages = pages)
  }
}
