package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Alto, Page, SpellingAlternative, SubsType, TextLine, Word}
import com.joliciel.jochre.ocr.core.utils.ImageUtils
import com.joliciel.jochre.search.core.lucene.{DocumentIndexInfo, JochreIndex}
import com.joliciel.jochre.search.core.{
  AggregationBins,
  AltoDocument,
  DocMetadata,
  DocReference,
  IndexField,
  SearchCriterion,
  SearchQuery
}
import org.apache.pdfbox.Loader
import org.apache.pdfbox.io.RandomAccessReadBuffer
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}
import org.slf4j.LoggerFactory
import zio.{&, Task, URIO, ZIO, ZLayer}

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color}
import java.io.{FileOutputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import javax.imageio.ImageIO
import scala.io.Source
import scala.util.Using
import scala.xml.{PrettyPrinter, XML}

trait SearchService {
  def indexPdf(
      ref: DocReference,
      pdfFile: InputStream,
      altoFile: InputStream,
      metadataFile: Option[InputStream]
  ): Task[Int]

  def indexAlto(ref: DocReference, alto: Alto, metadata: DocMetadata): Task[Int]

  def search(
      query: SearchQuery,
      first: Int = 0,
      max: Int = 100,
      maxSnippets: Option[Int] = None,
      rowPadding: Option[Int] = None,
      username: String = "unknown"
  ): Task[SearchResponse]

  def getImageSnippet(
      docId: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight]
  ): Task[BufferedImage]

  def aggregate(
      query: SearchQuery,
      field: IndexField,
      maxBins: Int
  ): Task[AggregationBins]

  def getTopAuthors(
      prefix: String,
      maxBins: Int
  ): Task[AggregationBins]
}

private[service] case class SearchServiceImpl(
    jochreIndex: JochreIndex,
    searchRepo: SearchRepo,
    metadataReader: MetadataReader = MetadataReader.default
) extends SearchService
    with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  override def indexPdf(
      ref: DocReference,
      pdfStream: InputStream,
      altoStream: InputStream,
      metadataStream: Option[InputStream]
  ): Task[Int] = {
    for {
      _ <- ZIO.attempt {
        // Create the directory to store the images and Alto
        val bookDir = ref.getBookDir()
        bookDir.toFile.mkdirs()
      }
      alto <- getAlto(ref, altoStream)
      pdfInfo <- getPdfInfo(ref, pdfStream)
      pdfMetadata = getMetadata(pdfInfo)
      metadata <- ZIO.attempt {
        val providedMetadata = metadataStream.map { metadataFile =>
          try {
            val contents = Source.fromInputStream(metadataFile, StandardCharsets.UTF_8.name()).getLines().mkString("\n")
            metadataReader.read(contents)
          } catch {
            case t: Throwable =>
              log.error("Unable to read metadata file", t)
              throw new BadMetadataFileFormat(t.getMessage)
          }
        }
        providedMetadata.getOrElse(pdfMetadata)
      }
      pageCount <- indexAlto(ref, alto, metadata)
    } yield pageCount
  }

  override def indexAlto(ref: DocReference, alto: Alto, metadata: DocMetadata): Task[Int] = {
    for {
      documentData <- persistDocument(ref, alto)
      pageCount <- ZIO.attempt {
        val document = AltoDocument(ref, documentData.docRev, documentData.text, metadata)
        val docInfo =
          DocumentIndexInfo(
            documentData.pageOffsets,
            documentData.newlineOffsets,
            documentData.hyphenatedWordOffsets,
            documentData.alternativesAtOffset
          )
        jochreIndex.addDocumentInfo(ref, docInfo)
        jochreIndex.indexer.indexDocument(document)
        val refreshed = jochreIndex.refresh
        if (log.isDebugEnabled) {
          log.debug(f"Index refreshed? $refreshed")
        }
        documentData.pageCount
      }
    } yield pageCount
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

  private def persistDocument(ref: DocReference, alto: Alto): Task[DocumentData] = {
    // We'll add the document reference at the start of the document text
    // This is because the Lucene Tokenizer is only aware of the text it is currently tokenizing,
    // and cannot be made aware of any context surrounding this text (e.g. the document reference).
    // However, before requesting the tokenization, we analyze the Alto file and construct a map of all synonyms
    // to add at a given offset (Alto ALTERNATIVE tags).
    // This map then needs to be used during tokenization to add the synonyms.
    // The tokenizer knows which synonyms to add via the document reference.
    val initialOffset = ref.ref.length + 1 // 1 for the newline
    for {
      docRev <- searchRepo.insertDocument(ref)
      documentData <- ZIO
        .iterate(alto.pages -> Seq.empty[PageData])(
          cont = { case (p, _) => p.nonEmpty }
        ) { case (pages, pageDataSeq) =>
          pages match {
            case page +: tail =>
              val startOffset = pageDataSeq.lastOption.map(_.endOffset).getOrElse(initialOffset)
              for {
                pageData <- persistPage(docRev, page, startOffset)
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
          val text = f"${ref.ref}\n${pageDataSeq.map(_.text).mkString}"
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

  private def persistPage(docId: DocRev, page: Page, startOffset: Int): Task[PageData] = {
    for {
      pageId <- searchRepo.insertPage(docId, page)
      textLineSeq <- ZIO.attempt {
        if (page.textLinesWithRectangles.nonEmpty) {
          page.textLinesWithRectangles
            .zip(
              page.allTextLines.tail.map(Some(_)) :+ None
            )
            .zipWithIndex
        } else {
          Seq.empty
        }
      }
      pageData <- ZIO
        .iterate(textLineSeq -> Seq.empty[RowData])(
          cont = { case (t, _) =>
            t.nonEmpty
          }
        ) { case (textLinesWithRectangles, rowDataSeq) =>
          textLinesWithRectangles match {
            case (((textLine, rect), nextTextLine), rowIndex) +: tail =>
              val currentStartOffset = rowDataSeq.lastOption.map(_.endOffset).getOrElse(startOffset)
              for {
                rowData <- persistRow(docId, pageId, textLine, nextTextLine, rowIndex, rect, currentStartOffset)
              } yield (tail, rowDataSeq :+ rowData)
          }
        }
        .mapAttempt { case (_, rowDataSeq) =>
          val alternativesAtOffset = rowDataSeq.foldLeft(Map.empty[Int, Seq[SpellingAlternative]]) {
            case (map, rowData) => map ++ rowData.alternativesAtOffset
          }
          val newlineOffsets = rowDataSeq.map(_.endOffset).toSet
          val finalOffset = rowDataSeq.lastOption.map(_.endOffset).getOrElse(startOffset)
          val text = rowDataSeq.map(_.text).mkString
          val hyphenatedWordOffsets = rowDataSeq.flatMap(_.hyphenatedWordOffset).toSet
          PageData(text, finalOffset, newlineOffsets, alternativesAtOffset, hyphenatedWordOffsets)
        }
    } yield pageData
  }

  private case class RowData(
      text: String,
      endOffset: Int,
      alternativesAtOffset: Map[Int, Seq[SpellingAlternative]],
      hyphenatedWordOffset: Option[Int]
  )

  private def persistRow(
      docId: DocRev,
      pageId: PageId,
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
      rowId <- searchRepo.insertRow(pageId, rowIndex, rowRectangle)
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
            case (word: Word) +: tail =>
              val hyphenatedOffset = word.subsType match {
                case Some(SubsType.HypPart1) =>
                  // At this point we know that any word with this attribute is the first word of a soft-hyphen compound
                  Some(offset + word.content.length + 1) // 1 for the newline character
                case _ => None
              }
              for {
                _ <- persistWord(docId, rowId, word, offset, hyphenatedOffset)
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

  private def getAlto(docRef: DocReference, altoStream: InputStream): Task[Alto] = {
    def acquire: Task[ZipInputStream] = ZIO
      .attempt { new ZipInputStream(altoStream) }
      .foldZIO(
        error => {
          log.error("Unable to open alto zip file", error)
          ZIO.fail(new BadAltoFileFormat(error.getMessage))
        },
        success => ZIO.succeed(success)
      )

    def release(zipInputStream: ZipInputStream): URIO[Any, Unit] = ZIO
      .attempt(zipInputStream.close())
      .orDieWith { ex =>
        log.error("Cannot close zip input stream for alto file", ex)
        ex
      }

    def readAlto(zipInputStream: ZipInputStream): Task[Alto] = ZIO
      .attempt {
        zipInputStream.getNextEntry
        val altoXml = XML.load(zipInputStream)

        // Store alto in content dir for future access
        val prettyPrinter = new PrettyPrinter(120, 2)
        val altoString = prettyPrinter.format(altoXml)
        val altoFile = docRef.getAltoPath()
        Using(new ZipOutputStream(new FileOutputStream(altoFile.toFile))) { zos =>
          zos.putNextEntry(new ZipEntry(f"${docRef.ref}_alto4.xml"))
          zos.write(altoString.getBytes(StandardCharsets.UTF_8))
          zos.flush()
        }

        val alto = Alto.fromXML(altoXml)
        alto
      }
      .foldZIO(
        error => {
          log.error("Unable to read alto zip file", error)
          ZIO.fail(new BadAltoFileFormat(error.getMessage))
        },
        success => ZIO.succeed(success)
      )

    ZIO.acquireReleaseWith(acquire)(release)(readAlto)
  }

  private case class PdfInfo(
      pageCount: Int,
      title: Option[String],
      author: Option[String],
      subject: Option[String],
      keywords: Option[String],
      creator: Option[String]
  )

  private def getMetadata(pdfInfo: PdfInfo): DocMetadata = {
    DocMetadata(
      title = pdfInfo.title,
      author = pdfInfo.author
    )
  }
  private def getPdfInfo(docRef: DocReference, pdfStream: InputStream): Task[PdfInfo] = {
    def acquire: Task[(InputStream, PDDocument)] = ZIO
      .attempt {
        val pdfBuffer = new RandomAccessReadBuffer(pdfStream)
        val pdf = Loader.loadPDF(pdfBuffer)
        (pdfStream, pdf)
      }
      .foldZIO(
        error => {
          log.error("Unable to open pdf file", error)
          ZIO.fail(new BadPdfFileFormat(error.getMessage))
        },
        success => ZIO.succeed(success)
      )

    val release: ((InputStream, PDDocument)) => URIO[Any, Unit] = { case (inputStream, pdf) =>
      ZIO
        .attempt {
          pdf.close()
          inputStream.close()
        }
        .orDieWith { ex =>
          log.error("Cannot close pdf file", ex)
          ex
        }
    }

    val readPdf: ((InputStream, PDDocument)) => Task[PdfInfo] = { case (_, pdf) =>
      ZIO
        .attempt {
          val pdfRenderer = new PDFRenderer(pdf)
          val docInfo = pdf.getDocumentInformation
          val pageCount = pdf.getNumberOfPages

          (1 to pageCount).foreach { i =>
            log.info(f"Extracting PDF page $i")
            val image = pdfRenderer.renderImageWithDPI(
              i - 1,
              300.toFloat,
              ImageType.RGB
            )
            val imageFile = docRef.getPageImagePath(i)
            ImageIO.write(image, "png", imageFile.toFile)
          }
          PdfInfo(
            pageCount,
            Option(docInfo.getTitle),
            Option(docInfo.getAuthor),
            Option(docInfo.getSubject),
            Option(docInfo.getKeywords),
            Option(docInfo.getCreator)
          )
        }
        .foldZIO(
          error => {
            log.error("Unable to read pdf file", error)
            ZIO.fail(new BadPdfFileFormat(error.getMessage))
          },
          success => ZIO.succeed(success)
        )
    }

    ZIO.acquireReleaseWith(acquire)(release)(readPdf)
  }

  override def search(
      query: SearchQuery,
      first: Int,
      max: Int,
      maxSnippets: Option[Int],
      rowPadding: Option[Int],
      username: String
  ): Task[SearchResponse] = {
    for {
      initialResponse <- ZIO.fromTry {
        Using(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher.search(query, first, max, maxSnippets, rowPadding)
        }
      }
      pages <- ZIO.foreach(initialResponse.results) { searchResult =>
        ZIO.foreach(searchResult.snippets) { snippet =>
          val page = searchRepo.getPageByStartOffset(searchResult.docRev, snippet.start)
          page
        }
      }
      responseWithPages <- ZIO.attempt {
        initialResponse.copy(results = initialResponse.results.zip(pages).map { case (searchResult, pages) =>
          searchResult.copy(snippets = searchResult.snippets.zip(pages).map { case (snippet, page) =>
            snippet.copy(page = page.map(_.index).getOrElse(-1))
          })
        })
      }
    } yield responseWithPages
  }

  override def aggregate(query: SearchQuery, field: IndexField, maxBins: Int): Task[AggregationBins] = ZIO.attempt {
    if (!field.aggregatable) {
      throw new IndexFieldNotAggregatable(f"Field ${field.entryName} is not aggregatable.")
    }
    Using(jochreIndex.searcherManager.acquire()) { searcher =>
      val bins = searcher.aggregate(query, field, maxBins)
      AggregationBins(bins)
    }.get
  }

  override def getTopAuthors(
      prefix: String,
      maxBins: Int
  ): Task[AggregationBins] = ZIO.fromTry {
    Using(jochreIndex.searcherManager.acquire()) { searcher =>
      val searchQuery = SearchQuery(SearchCriterion.StartsWith(IndexField.Author, prefix))
      val bins = searcher.aggregate(searchQuery, IndexField.Author, maxBins)
      AggregationBins(bins.sortBy(_.label))
    }
  }

  override def getImageSnippet(
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight]
  ): Task[BufferedImage] = {
    for {
      luceneDoc <- ZIO.attempt {
        Using(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher
            .getByDocRef(docRef)
            .getOrElse(throw new DocumentNotFoundInIndex(f"Document ${docRef.ref} not found in index"))
        }.get
      }
      startRowOpt <- searchRepo.getRowByStartOffset(luceneDoc.rev, startOffset)
      endRowOpt <- searchRepo.getRowByEndOffset(luceneDoc.rev, endOffset)
      _ <- ZIO.attempt {
        (startRowOpt, endRowOpt) match {
          case (None, _) =>
            throw new BadOffsetForImageSnippet(f"For document ${docRef.ref}, no start row at offset $startOffset")
          case (_, None) =>
            throw new BadOffsetForImageSnippet(f"For document ${docRef.ref}, no end row at offset $endOffset")
          case (Some(startRow), Some(endRow)) if startRow.pageId != endRow.pageId =>
            throw new BadOffsetForImageSnippet(
              f"In document ${docRef.ref}, start offset $startOffset is on different page to end offset $endOffset"
            )
          case _ => // everything's fine
        }
      }
      highlightWords <- ZIO.foreach(highlights) { highlight =>
        searchRepo
          .getWord(luceneDoc.rev, highlight.start)
          .mapAttempt(
            _.getOrElse(
              throw new BadOffsetForImageSnippet(
                f"For document ${docRef.ref}, no word to highlight at offset $startOffset"
              )
            )
          )
      }
      hyphenatedWords <- ZIO.foreach(highlightWords) { highlightWord =>
        highlightWord.hyphenatedOffset match {
          case Some(hyphenatedOffset) =>
            searchRepo.getWord(luceneDoc.rev, hyphenatedOffset)
          case None =>
            ZIO.succeed(None)
        }
      }
      startRow = startRowOpt.get
      endRow = endRowOpt.get
      page <- searchRepo.getPage(startRow.pageId)
      image <- ZIO.attempt {
        val pageImagePath = docRef.getPageImagePath(page.index)
        val originalImage = ImageIO.read(pageImagePath.toFile)
        val startRect = startRow.rect
        val endRect = endRow.rect

        val horizontalScale = originalImage.getWidth.toDouble / page.width.toDouble
        val verticalScale = originalImage.getHeight.toDouble / page.height.toDouble

        val initialSnippetRect = startRect.union(endRect)
        val snippetRect = Rectangle(
          left = (initialSnippetRect.left * horizontalScale).toInt - 5,
          top = (initialSnippetRect.top * verticalScale).toInt - 5,
          width = (initialSnippetRect.width * horizontalScale).toInt + 10,
          height = (initialSnippetRect.height * verticalScale).toInt + 10
        ).intersection(Rectangle(0, 0, originalImage.getWidth, originalImage.getHeight)).get

        val imageSnippet =
          new BufferedImage(snippetRect.width, snippetRect.height, BufferedImage.TYPE_INT_ARGB)

        val subImage = originalImage.getSubimage(
          snippetRect.left,
          snippetRect.top,
          snippetRect.width,
          snippetRect.height
        )
        val graphics2D = imageSnippet.createGraphics
        graphics2D.drawImage(subImage, 0, 0, subImage.getWidth, subImage.getHeight, null)
        val extra = 2

        val allWordsToHighlight = highlightWords ++ hyphenatedWords.flatten

        if (log.isDebugEnabled) {
          log.debug(f"Start row: $startRow")
          log.debug(f"Start row: $endRow")
          log.debug(f"Words to highlight: ${allWordsToHighlight.mkString(", ")}")
        }

        allWordsToHighlight.foreach { highlightWord =>
          val highlightRect = Rectangle(
            left = (highlightWord.rect.left * horizontalScale).toInt,
            top = (highlightWord.rect.top * verticalScale).toInt,
            width = (highlightWord.rect.width * horizontalScale).toInt,
            height = (highlightWord.rect.height * verticalScale).toInt
          ).intersection(Rectangle(0, 0, originalImage.getWidth, originalImage.getHeight)).get

          graphics2D.setStroke(new BasicStroke(1))
          graphics2D.setPaint(Color.BLACK)
          graphics2D.drawRect(
            highlightRect.left - snippetRect.left - extra,
            highlightRect.top - snippetRect.top - extra,
            highlightRect.width + (extra * 2),
            highlightRect.height + (extra * 2)
          )
          graphics2D.setColor(new Color(255, 255, 0, 127))
          graphics2D.fillRect(
            highlightRect.left - snippetRect.left - extra,
            highlightRect.top - snippetRect.top - extra,
            highlightRect.width + (extra * 2),
            highlightRect.height + (extra * 2)
          )
        }
        imageSnippet
      }
    } yield { image }
  }
}

object SearchService {
  lazy val live: ZLayer[JochreIndex & SearchRepo, Nothing, SearchService] =
    ZLayer {
      for {
        index <- ZIO.service[JochreIndex]
        searchRepo <- ZIO.service[SearchRepo]
      } yield SearchServiceImpl(index, searchRepo)
    }
}
