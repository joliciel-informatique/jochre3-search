package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{
  Alto,
  Hyphen,
  Page,
  SpellingAlternative,
  SubsType,
  TextBlock,
  TextLine,
  Word,
  WordOrSpace
}
import com.joliciel.jochre.ocr.core.utils.StringUtils
import com.joliciel.jochre.ocr.core.utils.StringUtils.stringToChars
import com.joliciel.jochre.search.core.{
  AltoDocument,
  DocMetadata,
  DocReference,
  PageNotFoundException,
  RowNotFoundException
}
import com.joliciel.jochre.search.core.lucene.{DocumentIndexInfo, JochreIndex}
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import zio.{Task, ZIO}

import java.io.{PrintWriter, StringWriter}

case class AltoIndexer(
    jochreIndex: JochreIndex,
    searchRepo: SearchRepo,
    suggestionRepo: SuggestionRepo,
    docRef: DocReference,
    username: String,
    ipAddress: Option[String],
    alto: Alto,
    metadata: DocMetadata,
    contentUpdated: Boolean = true,
    languageSpecificFilters: Option[LanguageSpecificFilters] = None
) {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.search")
  private val hyphenRegex = config.getString("hyphen-regex").r

  case class IndexData(
      docRev: DocRev,
      wordSuggestionRev: Option[WordSuggestionRev],
      metadataCorrectionRev: Option[MetadataCorrectionRev],
      pageCount: Int
  )

  def index(): Task[IndexData] = {
    for {
      _ <- ZIO.attempt {
        if (log.isDebugEnabled) {
          log.debug(f"About to process Alto for document ${docRef.ref}")
        }
      }
      suggestions <- suggestionRepo.getSuggestions(docRef)
      corrections <- suggestionRepo.getMetadataCorrections(docRef)
      documentData <- persistDocument(suggestions)
      _ <- searchRepo.updateDocumentStatus(documentData.docRev, DocumentStatus.Complete)
      pageCount <- ZIO
        .attempt {
          val correctedMetadata = corrections.foldLeft(metadata) { case (metadata, correction) =>
            log.debug(f"On doc ${docRef.ref}, correcting ${correction.field.entryName} to '${correction.newValue}'")
            correction.field.applyToMetadata(metadata, correction.newValue)
          }

          val ocrSoftware = alto.processingSteps.headOption.flatMap { step =>
            val software = Seq(step.softwareName, step.softwareVersion).flatten.mkString(" ")
            Option.when(software.nonEmpty)(software)
          }

          val document = AltoDocument(docRef, documentData.docRev, documentData.text, correctedMetadata, ocrSoftware)
          val docInfo =
            DocumentIndexInfo(
              documentData.pageOffsets,
              documentData.newlineOffsets,
              documentData.hyphenatedWordOffsets,
              documentData.alternativesAtOffset
            )
          jochreIndex.addDocumentInfo(docRef, docInfo)
          log.info(f"Finished processing Alto, about to index document ${docRef.ref}")
          jochreIndex.indexer.indexDocument(document)
          val refreshed = jochreIndex.refresh
          log.info(f"Finished indexing document ${docRef.ref}. Index refreshed? $refreshed")
          documentData.pageCount
        }
        .catchAll { case ex: Throwable =>
          val sw = new StringWriter();
          val pw = new PrintWriter(sw)
          ex.printStackTrace(pw)
          val messageWithStackTrace = f"${ex.getMessage}\n${sw.toString}"
          searchRepo
            .updateDocumentStatus(documentData.docRev, DocumentStatus.Failed(messageWithStackTrace))
            .foldZIO(
              failure => {
                log.error(f"Unable to mark document failure for ${docRef.ref}", failure)
                ZIO.fail(ex)
              },
              _ => {
                ZIO.fail(ex)
              }
            )
        }
      _ <- searchRepo.updateDocumentStatus(documentData.docRev, DocumentStatus.Indexed)
    } yield IndexData(
      docRev = documentData.docRev,
      wordSuggestionRev = suggestions.map(_.rev).maxOption(WordSuggestionRev.ordering),
      metadataCorrectionRev = corrections.map(_.rev).maxOption(MetadataCorrectionRev.ordering),
      pageCount = pageCount
    )
  }

  private case class DocumentData(
      docRev: DocRev,
      pageCount: Int,
      text: String,
      pageOffsets: Set[Int],
      newlineOffsets: Set[Int],
      alternativesAtOffset: Map[Int, Seq[SpellingAlternative]],
      hyphenatedWordOffsets: Set[Int]
  )

  private def persistDocument(
      suggestions: Seq[DbWordSuggestion]
  ): Task[DocumentData] = {
    // We'll add the document reference at the start of the document text
    // This is because the Lucene Tokenizer is only aware of the text it is currently tokenizing,
    // and cannot be made aware of any context surrounding this text (e.g. the document reference).
    // However, before requesting the tokenization, we analyze the Alto file and construct a map of all synonyms
    // to add at a given offset (Alto ALTERNATIVE tags).
    // This map then needs to be used during tokenization to add the synonyms.
    // The tokenizer knows which synonyms to add via the document reference.
    val initialOffset = docRef.ref.length + 1 // 1 for the newline
    for {
      docRev <-
        if (contentUpdated) {
          searchRepo.insertDocument(docRef, username, ipAddress)
        } else {
          searchRepo.getDocument(docRef).mapAttempt(_.rev)
        }
      documentData <- ZIO
        .iterate(alto.pages -> Seq.empty[PageData])(
          cont = { case (p, _) => p.nonEmpty }
        ) { case (pages, pageDataSeq) =>
          val pageSuggestionMap = suggestions.groupBy(_.pageIndex)
          pages match {
            case page +: tail =>
              log.info(f"For doc ${docRef.ref}, extracting page ${page.physicalPageNumber} of ${alto.pages.size}")
              val pageWithDefaultLanguage = page.withDefaultLanguage
              val startOffset = pageDataSeq.lastOption.map(_.endOffset).getOrElse(initialOffset)
              val pageSuggestions = pageSuggestionMap.getOrElse(page.physicalPageNumber, Seq.empty)
              val horizontalScale = page.width.toDouble / 10000.0
              val verticalScale = page.height.toDouble / 10000.0
              val scaledSuggestions = pageSuggestions.map(s =>
                s.copy(
                  left = (s.left * horizontalScale).toInt,
                  top = (s.top * verticalScale).toInt,
                  width = (s.width * horizontalScale).toInt,
                  height = (s.height * verticalScale).toInt
                )
              )
              for {
                pageData <- persistPage(docRev, pageWithDefaultLanguage, startOffset, scaledSuggestions)
              } yield (tail, pageDataSeq :+ pageData)
          }
        }
        .mapAttempt { case (_, pageDataSeq) =>
          val alternativesAtOffset = pageDataSeq.foldLeft(Map.empty[Int, Seq[SpellingAlternative]]) {
            case (map, pageData) => map ++ pageData.alternativesAtOffset
          }
          val pageOffsets = pageDataSeq.foldLeft(Set(initialOffset)) { case (pageOffsets, pageData) =>
            pageOffsets + pageData.endOffset
          }
          val newlineOffsets = pageDataSeq.foldLeft(Set(initialOffset)) { case (newlineOffsets, pageData) =>
            newlineOffsets ++ pageData.newlineOffsets
          }
          val hyphenatedWordOffsets = pageDataSeq.foldLeft(Set.empty[Int]) { case (hyphenatedWordOffsets, pageData) =>
            hyphenatedWordOffsets ++ pageData.hyphenatedWordOffsets
          }
          // As explained above, we add the document reference at the start of the text.
          val text = f"${docRef.ref}\n${pageDataSeq.map(_.text).mkString}"
          DocumentData(
            docRev,
            pageDataSeq.size,
            text,
            pageOffsets,
            newlineOffsets,
            alternativesAtOffset,
            hyphenatedWordOffsets
          )
        }
    } yield documentData
  }

  private case class PageData(
      text: String,
      endOffset: Int,
      newlineOffsets: Set[Int],
      alternativesAtOffset: Map[Int, Seq[SpellingAlternative]],
      hyphenatedWordOffsets: Set[Int]
  )

  private def persistPage(
      docRev: DocRev,
      page: Page,
      startOffset: Int,
      suggestions: Seq[DbWordSuggestion]
  ): Task[PageData] = {
    for {
      pageId <-
        if (contentUpdated) {
          searchRepo.insertPage(docRev, page, startOffset)
        } else {
          searchRepo
            .getPage(docRev, page.physicalPageNumber)
            .mapAttempt(
              _.map(_.id).getOrElse(throw new PageNotFoundException(docRef, pageNumber = page.physicalPageNumber))
            )
        }
      pageWithSuggestions <- ZIO.attempt { replaceSuggestions(page, suggestions) }
      pageData <- ZIO
        .iterate(pageWithSuggestions.allTextBlocks -> Seq.empty[TextBlockData])(
          cont = { case (t, _) =>
            t.nonEmpty
          }
        ) { case (textBoxes, textBlockDataSeq) =>
          textBoxes match {
            case textBlock +: tail =>
              val currentStartOffset = textBlockDataSeq.lastOption.map(_.endOffset).getOrElse(startOffset)
              val currentStartRowIndex = textBlockDataSeq.lastOption.map(_.endRowIndex).getOrElse(0)
              for {
                textBlockData <- persistTextBlock(
                  docRev,
                  page,
                  pageId,
                  textBlock,
                  currentStartRowIndex,
                  currentStartOffset
                )
              } yield (tail, textBlockDataSeq :+ textBlockData)
          }
        }
        .mapAttempt { case (_, textBlockDataSeq) =>
          val alternativesAtOffset = textBlockDataSeq.foldLeft(Map.empty[Int, Seq[SpellingAlternative]]) {
            case (map, textBlockData) => map ++ textBlockData.alternativesAtOffset
          }
          val newlineOffsets = textBlockDataSeq.flatMap(_.newlineOffsets).toSet
          if (log.isDebugEnabled) {
            log.debug(f"For page, found new line offsets at: ${newlineOffsets.mkString(", ")}")
          }
          val finalOffset = textBlockDataSeq.lastOption.map(_.endOffset).getOrElse(startOffset)
          val text = textBlockDataSeq.map(_.text).mkString
          val hyphenatedWordOffsets = textBlockDataSeq.flatMap(_.hyphenatedWordOffsets).toSet
          PageData(text, finalOffset, newlineOffsets, alternativesAtOffset, hyphenatedWordOffsets)
        }
    } yield pageData
  }

  private def replaceSuggestions(page: Page, suggestions: Seq[DbWordSuggestion]): Page = {
    page.transform { case textLine: TextLine =>
      replaceSuggestions(textLine, suggestions)
    }
  }

  private def replaceSuggestions(textLine: TextLine, suggestions: Seq[DbWordSuggestion]): TextLine = {
    if (textLine.wordsAndSpaces.isEmpty) {
      return textLine
    }
    val rowRectangle = textLine.wordsAndSpaces.map(_.rectangle).reduceLeft(_.union(_))
    val wordsAndSpaces = textLine.wordsAndSpaces
    val rowSuggestions = suggestions.filter(suggestion => suggestion.rect.intersection(rowRectangle).isDefined)
    val wordSuggestions = wordsAndSpaces.map { wordOrSpace =>
      // Since suggestions are ordered from newest to oldest, we want the first one that we find intersecting this word
      wordOrSpace -> rowSuggestions.find(suggestion =>
        wordOrSpace.rectangle.percentageIntersection(suggestion.rect) > 0.75
      )
    }.toMap
    val (withSuggestions, _) = wordsAndSpaces.foldLeft(Seq.empty[WordOrSpace] -> Option.empty[DbWordSuggestion]) {
      case ((withSuggestions, lastSuggestion), wordOrSpace) =>
        wordSuggestions.get(wordOrSpace).flatten match {
          case suggestion @ Some(_) if suggestion == lastSuggestion =>
            withSuggestions -> lastSuggestion
          case None =>
            (withSuggestions :+ wordOrSpace) -> None
          case Some(suggestion) =>
            val alternatives =
              languageSpecificFilters.map(_.getAlternatives(suggestion.suggestion)).getOrElse(Seq.empty)
            val newWord = Word(
              content = suggestion.suggestion,
              rectangle = suggestion.rect,
              glyphs = Seq.empty,
              alternatives = alternatives,
              confidence = 1.0
            )
            (withSuggestions :+ newWord) -> Some(suggestion)
        }
    }
    val endOfRowSuggestion = wordsAndSpaces.lastOption.flatMap(wordSuggestions.get).flatten
    val withHyphen = endOfRowSuggestion match {
      case Some(_) =>
        // A suggestion replaced the final word, we need to see if the suggestion ends in a hyphen
        val word = withSuggestions.collect { case w: Word => w }.last
        word.content match {
          case hyphenRegex(contentBeforeHyphen, hyphenContent) =>
            val contentChars = stringToChars(contentBeforeHyphen)
            val totalChars = contentChars.length + 1
            val width = word.rectangle.width
            val widthHyphen = width / totalChars
            val widthLetters = widthHyphen * (totalChars - 1)

            val wordAndHyphen = if (StringUtils.isLeftToRight(textLine.defaultLanguage.get)) {
              Seq(
                word.copy(
                  content = contentBeforeHyphen,
                  rectangle = word.rectangle.copy(
                    width = widthLetters
                  )
                ),
                Hyphen(
                  hyphenContent,
                  Rectangle(
                    word.rectangle.left + widthLetters,
                    word.rectangle.height,
                    widthHyphen,
                    word.rectangle.top
                  )
                )
              )
            } else {
              Seq(
                word.copy(
                  content = contentBeforeHyphen,
                  rectangle = word.rectangle.copy(
                    left = word.rectangle.left - widthHyphen,
                    width = widthLetters
                  )
                ),
                Hyphen(
                  hyphenContent,
                  Rectangle(
                    word.rectangle.left,
                    word.rectangle.height,
                    widthHyphen,
                    word.rectangle.top
                  )
                )
              )
            }
            withSuggestions.init ++ wordAndHyphen
          case _ => withSuggestions
        }
      case _ => withSuggestions
    }
    textLine.copy(wordsAndSpaces = withHyphen)
  }

  private case class TextBlockData(
      text: String,
      endOffset: Int,
      endRowIndex: Int,
      newlineOffsets: Set[Int],
      alternativesAtOffset: Map[Int, Seq[SpellingAlternative]],
      hyphenatedWordOffsets: Set[Int]
  )

  private def persistTextBlock(
      docRev: DocRev,
      page: Page,
      pageId: PageId,
      textBlock: TextBlock,
      startRowIndex: Int,
      startOffset: Int
  ): Task[TextBlockData] = {
    for {
      textLineSeq <- ZIO.attempt {
        if (textBlock.textLinesWithRectangles.nonEmpty) {
          textBlock.textLinesWithRectangles
            .zip(
              textBlock.textLines.tail.map(Some(_)) :+ None
            )
            .zipWithIndex
        } else {
          Seq.empty
        }
      }
      textBlockData <- ZIO
        .iterate(textLineSeq -> Seq.empty[RowData])(
          cont = { case (t, _) =>
            t.nonEmpty
          }
        ) { case (textLinesWithRectangles, rowDataSeq) =>
          textLinesWithRectangles match {
            case (((textLine, rect), nextTextLine), rowIndex) +: tail =>
              val currentStartOffset = rowDataSeq.lastOption.map(_.endOffset).getOrElse(startOffset)
              for {
                rowData <- persistRow(
                  docRev,
                  pageId,
                  page.physicalPageNumber,
                  textLine,
                  nextTextLine,
                  startRowIndex + rowIndex,
                  rect,
                  currentStartOffset
                )
              } yield (tail, rowDataSeq :+ rowData)
          }
        }
        .mapAttempt { case (_, rowDataSeq) =>
          val alternativesAtOffset = rowDataSeq.foldLeft(Map.empty[Int, Seq[SpellingAlternative]]) {
            case (map, rowData) => map ++ rowData.alternativesAtOffset
          }
          val finalOffset =
            rowDataSeq.lastOption.map(_.endOffset).getOrElse(startOffset) + 1 // 1 for the added newline character
          val newlineOffsets = if (rowDataSeq.isEmpty) {
            Set.empty[Int]
          } else {
            rowDataSeq.init.map(_.endOffset).toSet + finalOffset
          }
          if (log.isDebugEnabled) {
            log.debug(f"For text block, found new line offsets at: ${newlineOffsets.mkString(", ")}")
          }
          val text = rowDataSeq.map(_.text).mkString
          val hyphenatedWordOffsets = rowDataSeq.flatMap(_.hyphenatedWordOffset).toSet
          TextBlockData(
            text = text + "\n", // Add another newline at the end-of-paragraph
            endOffset = finalOffset,
            endRowIndex = startRowIndex + rowDataSeq.length,
            newlineOffsets,
            alternativesAtOffset,
            hyphenatedWordOffsets
          )
        }
    } yield textBlockData
  }

  private case class RowData(
      text: String,
      endOffset: Int,
      alternativesAtOffset: Map[Int, Seq[SpellingAlternative]],
      hyphenatedWordOffset: Option[Int]
  )

  private def persistRow(
      docRev: DocRev,
      pageId: PageId,
      pageNumber: Int,
      textLine: TextLine,
      nextTextLine: Option[TextLine],
      rowIndex: Int,
      rectangle: Rectangle,
      startOffset: Int
  ): Task[RowData] = {
    for {
      rowRectangle <- ZIO.attempt {
        // We take the union of all words and spaces if there are any
        if (textLine.wordsAndSpaces.isEmpty) {
          rectangle
        } else {
          textLine.wordsAndSpaces.map(_.rectangle).reduceLeft(_.union(_))
        }
      }
      rowId <-
        if (contentUpdated) {
          searchRepo.insertRow(pageId, rowIndex, rowRectangle)
        } else {
          searchRepo
            .getRow(docRev, pageNumber, rowIndex)
            .mapAttempt(_.map(_.id).getOrElse(throw new RowNotFoundException(docRef, pageNumber, rowIndex)))
        }
      wordsAndSpaces <- ZIO.attempt {
        (textLine.hyphen, nextTextLine.flatMap(_.words.headOption)) match {
          case (Some(hyphen), Some(nextWord)) =>
            // Combine the hyphen with the final word
            val withoutHyphen = textLine.wordsAndSpaces.init
            if (withoutHyphen.nonEmpty) {
              withoutHyphen.init ++ (withoutHyphen.last match {
                case lastWord: Word =>
                  val hardHyphen =
                    lastWord.subsContent match {
                      case Some(hyphenatedContent) =>
                        val totalHyphenCount =
                          hyphenatedContent.sliding(hyphen.content.length).count(window => window == hyphen.content)
                        val firstWordHyphenCount =
                          lastWord.content.sliding(hyphen.content.length).count(window => window == hyphen.content)
                        val secondWordHyphenCount =
                          nextWord.content.sliding(hyphen.content.length).count(window => window == hyphen.content)

                        if (log.isDebugEnabled) {
                          log.debug(f"Hyphenated content: $hyphenatedContent with $totalHyphenCount hyphens.")
                          log.debug(f"First word content: ${lastWord.content} with $firstWordHyphenCount hyphens.")
                          log.debug(f"Second word content: ${nextWord.content} with $secondWordHyphenCount hyphens.")
                          log.debug(
                            f"Is hard hyphen? ${totalHyphenCount > firstWordHyphenCount + secondWordHyphenCount}"
                          )
                        }
                        // If we have more hyphen characters in the hyphenated content than in the other two, we assume
                        // the added hyphen is a hard hyphen.
                        if (totalHyphenCount > firstWordHyphenCount + secondWordHyphenCount) {
                          true
                        } else {
                          false
                        }
                      case _ =>
                        // assume soft hyphen
                        false
                    }

                  // Hyphenated words should be handled differently depending on whether its a soft or hard hyphen
                  // If soft hyphen, we want to index a single word (and skip the hyphen in the content)
                  // If hard hyphen, we want to index two separate words
                  if (hardHyphen) {
                    Seq(lastWord.copy(subsType = None, subsContent = None), hyphen)
                  } else {
                    // This is a soft hyphen.
                    // - We need to ensure it will get indexed together with the following word as a single word.
                    // - We also need to associate this word in the database with a hyphenated offset, so that we
                    //   can highlight both rectangles when highlighting.
                    // For now, this is done by setting SubsType = HypPart1 and combining the word with the hyphen.
                    val lastWordWithHyphen = lastWord
                      .combineWith(hyphen)
                      .copy(subsType = Some(SubsType.HypPart1))
                    Seq(lastWordWithHyphen)
                  }
                case other => Seq(other, hyphen)
              })
            } else {
              Seq(hyphen)
            }
          case _ => textLine.wordsAndSpaces
        }
      }
      rowData <- ZIO
        .iterate(
          (wordsAndSpaces, "", startOffset, Option.empty[Int], Seq.empty[Option[(Int, Seq[SpellingAlternative])]])
        )(
          cont = { case (w, _, _, _, _) => w.nonEmpty }
        ) { case (wordsAndSpaces, text, offset, hyphenatedWordOffset, alternativeSeq) =>
          wordsAndSpaces match {
            case (word: Word) +: tail if word.content.nonEmpty =>
              val hyphenatedOffset = word.subsType match {
                case Some(SubsType.HypPart1) =>
                  // At this point we know that any word with this attribute is the first word of a soft-hyphen compound
                  Some(offset + word.content.length + 1) // 1 for the newline character
                case _ => None
              }
              for {
                _ <-
                  if (contentUpdated) {
                    persistWord(docRev, rowId, word, offset, hyphenatedOffset)
                  } else {
                    ZIO.succeed(())
                  }
              } yield (
                tail,
                text + word.content,
                offset + word.content.length,
                // If we have a hyphenated word offset, the current offset is hyphenated
                hyphenatedOffset.map(_ => offset),
                alternativeSeq :+ Option.when(word.alternatives.nonEmpty)(offset -> word.alternatives)
              )
            case other +: tail =>
              ZIO.succeed(
                (
                  tail,
                  text + other.content,
                  offset + other.content.length,
                  hyphenatedWordOffset,
                  alternativeSeq :+ None
                )
              )
          }
        }
        .mapAttempt { case (_, text, finalOffset, hyphenatedWordOffset, offsetAndAlternativeList) =>
          val alternativesAtOffset = offsetAndAlternativeList.flatten.toMap
          RowData(
            text = text + "\n",
            endOffset = finalOffset + 1, // 1 for the newline character
            alternativesAtOffset = alternativesAtOffset,
            hyphenatedWordOffset = hyphenatedWordOffset
          )
        }
    } yield rowData
  }

  private def persistWord(docId: DocRev, rowId: RowId, word: Word, offset: Int, hyphenatedOffset: Option[Int]) = {
    searchRepo.insertWord(docId, rowId, offset, hyphenatedOffset, word)
  }
}
