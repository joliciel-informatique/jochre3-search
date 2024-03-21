package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Alto, Page, SpellingAlternative, SubsType, TextLine, Word}
import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.{AltoDocument, DocMetadata, DocReference}
import org.apache.pdfbox.Loader
import org.apache.pdfbox.io.RandomAccessReadBuffer
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.rendering.PDFRenderer
import org.slf4j.LoggerFactory
import zio.{&, Task, URIO, ZIO, ZLayer}

import java.awt.image.BufferedImage
import java.io.{File, FileInputStream}
import java.util.zip.ZipInputStream
import scala.util.Using
import scala.xml.XML

trait SearchService {
  def indexPdf(id: DocReference, pdfFile: File, altoFile: File, metadataFile: Option[File]): Task[String]

  def indexAlto(ref: DocReference, alto: Alto, metadata: DocMetadata): Task[String]

  def search(query: String): Task[Unit]
}

private[service] case class SearchServiceImpl(jochreIndex: JochreIndex, searchRepo: SearchRepo) extends SearchService {
  private val log = LoggerFactory.getLogger(getClass)

  override def indexPdf(ref: DocReference, pdfFile: File, altoFile: File, metadataFile: Option[File]): Task[String] = {
    for {
      alto <- getAlto(altoFile)
      pdfInfo <- getPdfInfo(pdfFile)
      metadata = getMetadata(ref, pdfInfo)
      text <- indexAlto(ref, alto, metadata)
    } yield text
  }

  override def indexAlto(ref: DocReference, alto: Alto, metadata: DocMetadata): Task[String] = {
    for {
      documentData <- persistDocument(ref, alto)
      text <- ZIO.attempt {
        val document = AltoDocument(ref, documentData.text, metadata)
        jochreIndex.addAlternatives(ref, documentData.alternativesAtOffset)
        jochreIndex.indexer.indexDocument(document)
        jochreIndex.refresh
        document.text
      }
    } yield text
  }
  private case class DocumentData(text: String, alternativesAtOffset: Map[Int, Seq[SpellingAlternative]])

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
      docId <- searchRepo.insertDocument(ref)
      documentData <- ZIO
        .iterate(alto.pages -> Seq.empty[PageData])(
          cont = { case (p, _) => p.nonEmpty }
        ) { case (pages, pageDataSeq) =>
          pages match {
            case page +: tail =>
              val startOffset = pageDataSeq.lastOption.map(_.endOffset).getOrElse(initialOffset)
              for {
                pageData <- persistPage(docId, page, startOffset)
              } yield (tail, pageDataSeq :+ pageData)
          }
        }
        .mapAttempt { case (_, pageDataSeq) =>
          val alternativesAtOffset = pageDataSeq.foldLeft(Map.empty[Int, Seq[SpellingAlternative]]) {
            case (map, pageData) => map ++ pageData.alternativesAtOffset
          }
          // As explained above, we add the document reference at the start of the text.
          val text = f"${ref.ref}\n${pageDataSeq.map(_.text).mkString}"
          DocumentData(text, alternativesAtOffset)
        }
    } yield documentData
  }

  private case class PageData(text: String, endOffset: Int, alternativesAtOffset: Map[Int, Seq[SpellingAlternative]])

  private def persistPage(docId: DocId, page: Page, startOffset: Int): Task[PageData] = {
    for {
      pageId <- searchRepo.insertPage(docId, page)
      pageData <- ZIO
        .iterate(page.textLinesWithRectangles.zipWithIndex -> Seq.empty[RowData])(
          cont = { case (t, _) =>
            t.nonEmpty
          }
        ) { case (textLinesWithRectangles, rowDataSeq) =>
          textLinesWithRectangles match {
            case ((textLine, rect), rowIndex) +: tail =>
              val currentStartOffset = rowDataSeq.lastOption.map(_.endOffset).getOrElse(startOffset)
              for {
                rowData <- persistRow(docId, pageId, textLine, rowIndex, rect, currentStartOffset)
              } yield (tail, rowDataSeq :+ rowData)
          }
        }
        .mapAttempt { case (_, rowDataSeq) =>
          val alternativesAtOffset = rowDataSeq.foldLeft(Map.empty[Int, Seq[SpellingAlternative]]) {
            case (map, rowData) => map ++ rowData.alternativesAtOffset
          }
          val finalOffset = rowDataSeq.lastOption.map(_.endOffset).getOrElse(startOffset)
          val text = rowDataSeq.map(_.text).mkString
          PageData(text, finalOffset, alternativesAtOffset)
        }
    } yield pageData
  }

  private case class RowData(text: String, endOffset: Int, alternativesAtOffset: Map[Int, Seq[SpellingAlternative]])

  private def persistRow(
      docId: DocId,
      pageId: PageId,
      textLine: TextLine,
      rowIndex: Int,
      rectangle: Rectangle,
      startOffset: Int
  ): Task[RowData] = {
    for {
      rowId <- searchRepo.insertRow(pageId, rowIndex, rectangle)
      wordsAndSpaces <- ZIO.attempt {
        textLine.hyphen match {
          case Some(hyphen) =>
            // Combine the hyphen with the final word
            val withoutHyphen = textLine.wordsAndSpaces.init
            if (!withoutHyphen.isEmpty) {
              withoutHyphen.init ++ (withoutHyphen.last match {
                case lastWord: Word =>
                  val lastWordWithHyphen = lastWord
                    .combineWith(hyphen)
                    .copy(subsType = Some(SubsType.HypPart1), content = lastWord.content + hyphen.content)
                  Seq(lastWordWithHyphen)
                case other => Seq(other, hyphen)
              })
            } else {
              Seq(hyphen)
            }
          case None => textLine.wordsAndSpaces
        }
      }
      rowData <- ZIO
        .iterate((wordsAndSpaces, "", startOffset, Seq.empty[Option[(Int, Seq[SpellingAlternative])]]))(
          cont = { case (w, _, _, _) => w.nonEmpty }
        ) { case (wordsAndSpaces, text, offset, alternativeSeq) =>
          wordsAndSpaces match {
            case (word: Word) +: tail =>
              val hyphenatedOffset = word.subsType match {
                case Some(SubsType.HypPart1) => Some(offset + word.content.length + 1) // 1 for the newline character
                case _                       => None
              }
              for {
                _ <- persistWord(docId, rowId, word, offset, hyphenatedOffset)
              } yield (
                tail,
                text + word.content,
                offset + word.content.length,
                alternativeSeq :+ Option.when(word.alternatives.nonEmpty)(offset -> word.alternatives)
              )
            case other +: tail =>
              ZIO.succeed((tail, text + other.content, offset + other.content.length, alternativeSeq :+ None))
          }
        }
        .mapAttempt { case (_, text, finalOffset, offsetAndAlternativeList) =>
          val alternativesAtOffset = offsetAndAlternativeList.flatten.toMap
          RowData(text + "\n", finalOffset + 1, alternativesAtOffset) // 1 for the newline character
        }
    } yield rowData
  }

  private def persistWord(docId: DocId, rowId: RowId, word: Word, offset: Int, hyphenatedOffset: Option[Int]) = {
    searchRepo.insertWord(docId, rowId, offset, hyphenatedOffset, word)
  }

  private def getAlto(altoZipFile: File): Task[Alto] = {
    def acquire: Task[ZipInputStream] = ZIO.attempt { new ZipInputStream(new FileInputStream(altoZipFile)) }
    def release(zipInputStream: ZipInputStream): URIO[Any, Unit] = ZIO
      .attempt(zipInputStream.close())
      .orDieWith { ex =>
        log.error("Cannot close zip input stream for alto file", ex)
        ex
      }

    def readAlto(zipInputStream: ZipInputStream): Task[Alto] = ZIO.attempt {
      zipInputStream.getNextEntry
      val altoXml = XML.load(zipInputStream)
      val alto = Alto.fromXML(altoXml)
      alto
    }

    ZIO.acquireReleaseWith(acquire)(release)(readAlto)
  }

  private case class PdfInfo(
      pageCount: Int,
      title: Option[String],
      author: Option[String],
      subject: Option[String],
      keywords: Option[String],
      creator: Option[String],
      images: Seq[BufferedImage]
  )

  private def getMetadata(docRef: DocReference, pdfInfo: PdfInfo): DocMetadata = {
    DocMetadata(
      title = pdfInfo.title.getOrElse(docRef.ref),
      author = pdfInfo.author
    )
  }
  private def getPdfInfo(pdfFile: File): Task[PdfInfo] = {
    def acquire: Task[(FileInputStream, PDDocument)] = ZIO.attempt {
      val inputStream = new FileInputStream(pdfFile)
      val pdfStream = new RandomAccessReadBuffer(inputStream)
      val pdf = Loader.loadPDF(pdfStream)
      (inputStream, pdf)
    }

    val release: ((FileInputStream, PDDocument)) => URIO[Any, Unit] = { case (inputStream, pdf) =>
      ZIO
        .attempt {
          pdf.close()
          inputStream.close()
        }
        .orDieWith { ex =>
          log.error("Cannot close document", ex)
          ex
        }
    }

    val readPdf: ((FileInputStream, PDDocument)) => Task[PdfInfo] = { case (_, pdf) =>
      ZIO.attempt {
        val pdfRenderer = new PDFRenderer(pdf)
        val docInfo = pdf.getDocumentInformation
        val pageCount = pdf.getNumberOfPages

        val images = (1 to pageCount).map { i =>
          pdfRenderer.renderImage(i)
        }
        PdfInfo(
          pageCount,
          Option(docInfo.getTitle),
          Option(docInfo.getAuthor),
          Option(docInfo.getSubject),
          Option(docInfo.getKeywords),
          Option(docInfo.getCreator),
          images
        )
      }
    }

    ZIO.acquireReleaseWith(acquire)(release)(readPdf)
  }

  override def search(query: String): Task[Unit] = ZIO.fromTry {
    Using(jochreIndex.searcherManager.acquire()) { searcher => }
  }
}

object SearchService {
  lazy val live: ZLayer[JochreIndex & SearchRepo, Nothing, SearchService] =
    ZLayer.fromFunction(SearchServiceImpl(_, _))
}
