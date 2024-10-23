package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model._
import com.joliciel.jochre.ocr.core.utils.ImageUtils
import com.joliciel.jochre.search.core._
import com.joliciel.jochre.search.core.lucene.{IndexTerm, JochreIndex, TermLister}
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import com.typesafe.config.ConfigFactory
import org.apache.pdfbox.Loader
import org.apache.pdfbox.io.RandomAccessReadBuffer
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}
import org.slf4j.LoggerFactory
import zio.stream.{ZSink, ZStream}
import zio.{&, Task, URIO, ZIO, ZLayer}

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color}
import java.io.{BufferedWriter, FileInputStream, FileOutputStream, FileWriter, InputStream, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import javax.imageio.ImageIO
import scala.io.Source
import scala.util.Using
import scala.xml.{Node, PrettyPrinter, XML}

trait SearchService {

  def addNewDocumentAsPdf(
      ref: DocReference,
      username: String,
      ipAddress: Option[String],
      pdfStream: InputStream,
      altoStream: InputStream,
      metadataStream: Option[InputStream]
  ): Task[Int]

  def addNewDocumentAsImages(
      ref: DocReference,
      username: String,
      ipAddress: Option[String],
      imagesZipStream: InputStream,
      altoStream: InputStream,
      metadataStream: Option[InputStream]
  ): Task[Int]

  private[service] def addFakeDocument(
      ref: DocReference,
      username: String,
      ipAddress: Option[String],
      alto: Alto,
      metadata: DocMetadata
  ): Task[Int]

  def removeDocument(
      ref: DocReference
  ): Task[Unit]

  def updateAlto(
      ref: DocReference,
      altoStream: InputStream
  ): Task[Unit]

  def updateMetadata(
      ref: DocReference,
      metadataStream: InputStream
  ): Task[Unit]

  def search(
      query: SearchQuery,
      sort: Sort = Sort.Score,
      first: Int = 0,
      max: Int = 100,
      maxSnippets: Option[Int] = None,
      rowPadding: Option[Int] = None,
      username: String = "unknown",
      ipAddress: Option[String] = None,
      addOffsets: Boolean = true,
      physicalNewLines: Boolean = true
  ): Task[SearchResponse]

  def list(
      query: SearchQuery,
      sort: Sort = Sort.Score
  ): Task[Seq[DocReference]]

  def getImageSnippet(
      docId: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight]
  ): Task[BufferedImage]

  def getImageSnippetAndHighlights(
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight]
  ): Task[(BufferedImage, Seq[Rectangle])]

  /** Aggregate the results of a given query by a given field.
    * @param query
    *   which query to use for aggregating the results
    * @param field
    *   which field to aggregate on
    * @param maxBins
    *   if provided, a maximum of maxBins bins will be returned (by descending count)
    * @param sortByLabel
    *   if true, bins will be sorted by label after limiting to max bins, otherwise bins are sorted by descending
    *   count
    * @return
    */
  def aggregate(
      query: SearchQuery,
      field: IndexField,
      maxBins: Option[Int],
      sortByLabel: Boolean = false
  ): Task[AggregationBins]

  def getTopAuthors(
      prefix: String,
      maxBins: Option[Int],
      includeAuthorField: Boolean,
      includeAuthorInTranscriptionField: Boolean
  ): Task[AggregationBins]

  def getTextAsHtml(
      docRef: DocReference
  ): Task[String]

  def getWordText(
      docRef: DocReference,
      wordOffset: Int
  ): Task[String]

  def getWordImage(
      docRef: DocReference,
      wordOffset: Int
  ): Task[BufferedImage]

  def getIndexSize(): Task[Int]

  def suggestWord(
      username: String,
      ipAddress: Option[String],
      docRef: DocReference,
      wordOffset: Int,
      suggestion: String
  ): Task[Unit]

  def correctMetadata(
      username: String,
      ipAddress: Option[String],
      docRef: DocReference,
      field: MetadataField,
      newValue: String,
      applyEverywhere: Boolean
  ): Task[MetadataCorrectionId]

  def undoMetadataCorrection(
      id: MetadataCorrectionId
  ): Task[Seq[DocReference]]

  def reindex(
      docRef: DocReference
  ): Task[Int]

  /** Returns false if re-indexing was already underway, true otherwise (after re-indexing).
    */
  def reindexWhereRequired(): Task[Boolean]

  private[service] def storeAlto(docRef: DocReference, altoXml: Node): Unit

  def getTerms(docRef: DocReference): Task[Map[String, Seq[IndexTerm]]]

  def markForReindex(docRef: DocReference): Task[Unit]

  def markAllForReindex(): Task[Unit]

  def cleanUpAtStartUp(): Task[Unit]
}

private[service] case class SearchServiceImpl(
    jochreIndex: JochreIndex,
    searchRepo: SearchRepo,
    suggestionRepo: SuggestionRepo,
    metadataReader: MetadataReader = MetadataReader.default,
    languageSpecificFilters: Option[LanguageSpecificFilters] = None
) extends SearchService
    with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.search")
  private val indexParallelism = config.getInt("index-parallelism")
  private val snippetClass = config.getString("snippet-class")

  private val reindexingUnderway: AtomicBoolean = new AtomicBoolean(false)
  private val documentsBeingIndexed = new ConcurrentHashMap[DocReference, Boolean]()

  override def addNewDocumentAsPdf(
      ref: DocReference,
      username: String,
      ipAddress: Option[String],
      pdfStream: InputStream,
      altoStream: InputStream,
      metadataStream: Option[InputStream]
  ): Task[Int] = {
    for {
      _ <- ZIO.attempt {
        // Ensure book isn't already in the index
        Using.resource(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher.getByDocRef(ref).foreach(_ => throw new DocumentAlreadyInIndexException(ref))
        }
        // Create the directory to store the images and Alto
        val bookDir = ref.getBookDir()
        bookDir.toFile.mkdirs()
      }
      alto <- readAndStoreAlto(ref, altoStream)
      pdfInfo <- readPdfInfo(ref, pdfStream)
      metadata <- ZIO.attempt {
        metadataStream
          .map(readAndStoreMetadata(ref, _))
          .getOrElse(pdfInfo.metadata)
      }
      pageCount <- indexAlto(ref, username, ipAddress, alto, metadata)
    } yield pageCount
  }

  def addNewDocumentAsImages(
      ref: DocReference,
      username: String,
      ipAddress: Option[String],
      imagesZipStream: InputStream,
      altoStream: InputStream,
      metadataStream: Option[InputStream]
  ): Task[Int] = {
    for {
      _ <- ZIO.attempt {
        // Ensure book isn't already in the index
        Using.resource(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher.getByDocRef(ref).foreach(_ => throw new DocumentAlreadyInIndexException(ref))
        }
        // Create the directory to store the images and Alto
        val bookDir = ref.getBookDir()
        bookDir.toFile.mkdirs()
      }
      alto <- readAndStoreAlto(ref, altoStream)
      _ <- extractImages(ref, imagesZipStream)
      metadata <- ZIO.attempt {
        metadataStream
          .map(readAndStoreMetadata(ref, _))
          .getOrElse(DocMetadata())
      }
      pageCount <- indexAlto(ref, username, ipAddress, alto, metadata)
    } yield pageCount
  }

  def removeDocument(
      ref: DocReference
  ): Task[Unit] = {
    for {
      _ <- ZIO.attempt {
        jochreIndex.deleteDocument(ref)
        val refreshed = jochreIndex.refresh
        if (log.isDebugEnabled) {
          log.debug(f"Index refreshed after delete? $refreshed")
        }
        val bookDir = ref.getBookDir()
        if (bookDir.toFile.exists() && bookDir.toFile.isDirectory) {
          val files = Option(bookDir.toFile.list()).getOrElse(Array.empty)
          files.foreach { fileName =>
            val currentFile = bookDir.resolve(fileName).toFile
            currentFile.delete()
          }
        }
        bookDir.toFile.delete()
      }
      dbDoc <- searchRepo.getDocument(ref)
      _ <- searchRepo.deleteDocument(dbDoc.rev)
    } yield ()
  }

  override def updateAlto(
      ref: DocReference,
      altoStream: InputStream
  ): Task[Unit] = {
    for {
      _ <- ZIO.attempt {
        // Ensure book is in index
        Using.resource(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher
            .getByDocRef(ref)
            .getOrElse(
              throw new DocumentNotFoundInIndex(ref)
            )
        }
      }
      _ <- readAndStoreAlto(ref, altoStream)
      _ <- markForReindex(ref)
    } yield ()
  }

  override def updateMetadata(
      ref: DocReference,
      metadataStream: InputStream
  ): Task[Unit] = {
    for {
      _ <- ZIO.attempt {
        // Ensure book is in index
        Using.resource(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher
            .getByDocRef(ref)
            .getOrElse(
              throw new DocumentNotFoundInIndex(ref)
            )
        }
      }
      _ <- ZIO.attempt(readAndStoreMetadata(ref, metadataStream))
      _ <- markForReindex(ref)
    } yield ()
  }

  override def addFakeDocument(
      ref: DocReference,
      username: String,
      ipAddress: Option[String],
      alto: Alto,
      metadata: DocMetadata
  ): Task[Int] =
    for {
      _ <- ZIO.attempt {
        ref.getBookDir().toFile.mkdirs()
        storeAlto(ref, alto.toXml)
        storeMetadata(ref, metadata)
      }
      pages <- indexAlto(ref, username, ipAddress, alto, metadata)
    } yield pages

  private def indexAlto(
      docRef: DocReference,
      username: String,
      ipAddress: Option[String],
      alto: Alto,
      metadata: DocMetadata,
      altoUpdated: Boolean = true
  ): Task[Int] = {

    val acquire = ZIO.attempt {
      val alreadyUnderway = Option(documentsBeingIndexed.putIfAbsent(docRef, true)).isDefined
      alreadyUnderway
    }

    def release(underway: Boolean) = ZIO.succeed {
      if (!underway) {
        documentsBeingIndexed.remove(docRef)
        log.info(f"Finished indexing document ${docRef.ref}")
      }
    }

    def run(underway: Boolean) = {
      if (underway) {
        log.info(f"Document ${docRef.ref} already being indexed")

        ZIO.succeed(0)
      } else {
        log.info(f"Re-indexing ${docRef.ref}, altoUpdated? $altoUpdated")
        val altoIndexer =
          AltoIndexer(
            jochreIndex,
            searchRepo,
            suggestionRepo,
            docRef,
            username,
            ipAddress,
            alto,
            metadata,
            altoUpdated,
            languageSpecificFilters
          )

        for {
          indexData <- altoIndexer.index()
          _ <- ZIO.succeed(
            log.info(
              f"Updating indexed document for ${docRef.ref}: docRev: ${indexData.docRev.rev}, wordSuggestionRev: ${indexData.wordSuggestionRev
                .map(_.rev)}, corrections: ${indexData.corrections.map(c => f"(${c.rev.rev} => ${c.field.entryName})").mkString(", ")}"
            )
          )
          _ <- searchRepo.upsertIndexedDocument(
            docRef,
            indexData.docRev,
            indexData.wordSuggestionRev,
            indexData.corrections,
            reindex = false
          )
        } yield indexData.pageCount
      }
    }

    ZIO.acquireReleaseWith(acquire)(release)(run)
  }

  private def readAndStoreMetadata(ref: DocReference, metadataStream: InputStream): DocMetadata =
    try {
      val contents = Source.fromInputStream(metadataStream, StandardCharsets.UTF_8.name()).getLines().mkString("\n")
      val metadata = metadataReader.read(contents)

      storeMetadata(ref, contents)

      metadata
    } catch {
      case t: Throwable =>
        log.error("Unable to read metadata file", t)
        throw new BadMetadataFileFormat(t.getMessage)
    }

  private def storeMetadata(ref: DocReference, metadata: DocMetadata): Unit = {
    storeMetadata(ref, metadataReader.write(metadata))
  }

  private def storeMetadata(ref: DocReference, metadata: String): Unit = {
    // Write the metadata to the content directory, so we can re-read it later
    val metadataPath = ref.getMetadataPath()
    Using(new BufferedWriter(new FileWriter(metadataPath.toFile, StandardCharsets.UTF_8))) { bw =>
      bw.write(metadata)
    }.get
  }

  private[service] def storeAlto(docRef: DocReference, altoXml: Node): Unit = {
    // Store alto in content dir for future access
    val prettyPrinter = new PrettyPrinter(120, 2)
    val altoString = prettyPrinter.format(altoXml)
    val altoFile = docRef.getAltoPath()
    Using(new ZipOutputStream(new FileOutputStream(altoFile.toFile))) { zos =>
      zos.putNextEntry(new ZipEntry(f"${docRef.ref}_alto4.xml"))
      zos.write(altoString.getBytes(StandardCharsets.UTF_8))
      zos.flush()
    }.get
  }

  private def readAndStoreAlto(docRef: DocReference, altoStream: InputStream): Task[Alto] = {
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

        storeAlto(docRef, altoXml)

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
  ) {
    val metadata: DocMetadata = DocMetadata(
      title = title,
      author = author
    )
  }

  private def readPdfInfo(docRef: DocReference, pdfStream: InputStream): Task[PdfInfo] = {
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
            log.info(f"Extracting PDF page $i of $pageCount for ${docRef.ref}")
            val image = pdfRenderer.renderImageWithDPI(
              i - 1,
              300.toFloat,
              ImageType.RGB
            )
            val imageFile = docRef.getPageImagePath(i)
            val extension = if (imageFile.toFile.getName.endsWith("png")) {
              "png"
            } else {
              "jpg"
            }
            ImageIO.write(image, extension, imageFile.toFile)
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

  private def extractImages(docRef: DocReference, imagesZipStream: InputStream): Task[Unit] = {
    def acquire: Task[ZipInputStream] = ZIO
      .attempt { new ZipInputStream(imagesZipStream) }
      .foldZIO(
        error => {
          log.error("Unable to open zip file of images", error)
          ZIO.fail(new BadImageZipFileFormat(error.getMessage))
        },
        success => ZIO.succeed(success)
      )

    def release(zipInputStream: ZipInputStream): URIO[Any, Unit] = ZIO
      .attempt(zipInputStream.close())
      .orDieWith { ex =>
        log.error("Cannot close zip input stream of images", ex)
        ex
      }

    def readImages(zipInputStream: ZipInputStream): Task[Unit] = ZIO
      .attempt {
        val bookDir = docRef.getBookDir()
        log.info(f"About to read image zip file for ${docRef.ref}")
        Iterator
          .continually(Option(zipInputStream.getNextEntry))
          .takeWhile(_.isDefined)
          .foreach {
            case Some(zipEntry) =>
              val image = ImageIO.read(zipInputStream)
              val imageFile = bookDir.resolve(zipEntry.getName).toFile
              log.info(f"Writing ${imageFile.getName}")
              val extension = if (imageFile.getName.endsWith("png")) {
                "png"
              } else {
                "jpg"
              }
              ImageIO.write(image, extension, imageFile)
            case None => // Can never happen
          }
      }
      .foldZIO(
        error => {
          log.error("Unable to read image zip file", error)
          ZIO.fail(new BadImageZipFileFormat(error.getMessage))
        },
        _ => ZIO.succeed(())
      )

    ZIO.acquireReleaseWith(acquire)(release)(readImages)
  }

  override def search(
      query: SearchQuery,
      sort: Sort,
      first: Int,
      max: Int,
      maxSnippets: Option[Int],
      rowPadding: Option[Int],
      username: String,
      ipAddress: Option[String],
      addOffsets: Boolean,
      physicalNewLines: Boolean
  ): Task[SearchResponse] = {
    for {
      initialResponse <- ZIO.fromTry {
        Using(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher.search(query, sort, first, max, maxSnippets, rowPadding, addOffsets)
        }
      }
      _ <- searchRepo.insertQuery(
        username,
        ipAddress,
        query.criterion,
        sort,
        first,
        max,
        initialResponse.totalCount.toInt
      )
      pages <- ZIO.foreach(initialResponse.results) { searchResult =>
        ZIO.foreach(searchResult.snippets) { snippet =>
          val page = searchRepo.getPageByWordOffset(searchResult.docRev, snippet.start)
          page
        }
      }
      responseWithPages <- ZIO.attempt {
        initialResponse.copy(results = initialResponse.results.zip(pages).map { case (searchResult, pages) =>
          val bookUrl = searchResult.metadata.getBookUrl(searchResult.docRef)

          searchResult.copy(
            metadata = searchResult.metadata.copy(url = bookUrl),
            snippets = searchResult.snippets.zip(pages).map { case (snippet, page) =>
              val pageNumber = page.map(_.index)
              val deepLink = pageNumber.flatMap(searchResult.metadata.getDeepLink(searchResult.docRef, _))
              val innerTextWithDivs = snippet.text.replaceAll("<br><br>", f"""</div><div class="$snippetClass">""")
              val textWithDivs = f"""<div class="$snippetClass">$innerTextWithDivs</div>"""
              snippet.copy(
                text = if (physicalNewLines) {
                  textWithDivs
                } else {
                  textWithDivs.replaceAll("<br>", " ")
                },
                page = pageNumber.getOrElse(-1),
                deepLink = deepLink
              )
            }
          )
        })
      }
    } yield responseWithPages
  }

  override def list(
      query: SearchQuery,
      sort: Sort
  ): Task[Seq[DocReference]] = {
    for {
      docRefs <- ZIO.fromTry {
        Using(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher.findMatchingRefs(query, sort = sort)
        }
      }
    } yield docRefs
  }

  override def aggregate(
      query: SearchQuery,
      field: IndexField,
      maxBins: Option[Int],
      sortByLabel: Boolean
  ): Task[AggregationBins] =
    ZIO.attempt {
      if (!field.aggregatable) {
        throw new IndexFieldNotAggregatable(f"Field ${field.entryName} is not aggregatable.")
      }
      Using(jochreIndex.searcherManager.acquire()) { searcher =>
        val bins = searcher.aggregate(query, field, maxBins)
        val sortedBins = if (sortByLabel) {
          bins.sortBy(_.label)
        } else {
          bins
        }
        AggregationBins(sortedBins)
      }.get
    }

  def getTopAuthors(
      prefix: String,
      maxBins: Option[Int],
      includeAuthorField: Boolean,
      includeAuthorInTranscriptionField: Boolean
  ): Task[AggregationBins] = ZIO.fromTry {
    val fields = (Seq.empty[Option[IndexField]] :+
      Option.when(includeAuthorField)(IndexField.Author) :+
      Option.when(includeAuthorInTranscriptionField)(IndexField.AuthorEnglish)).flatten

    if (fields.isEmpty) {
      throw new NoFieldRequestedForAggregation()
    }

    Using(jochreIndex.searcherManager.acquire()) { searcher =>
      val binsByCount = fields
        .map { field =>
          val query = SearchQuery(SearchCriterion.StartsWith(field, prefix))
          searcher.aggregate(query, field, maxBins)
        }
        .flatten
        .sortBy(0 - _.count)

      val limited = maxBins.map(binsByCount.take(_)).getOrElse(binsByCount)

      val binsByLabel = limited
        .sortBy(_.label)

      AggregationBins(binsByLabel)
    }
  }

  override def getImageSnippet(
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight]
  ): Task[BufferedImage] = {
    getImageAndHighlightRects(docRef, startOffset, endOffset, highlights, drawHighlights = true).map(_._1)
  }

  override def getImageSnippetAndHighlights(
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight]
  ): Task[(BufferedImage, Seq[Rectangle])] = {
    getImageAndHighlightRects(docRef, startOffset, endOffset, highlights, drawHighlights = false)
  }

  private def getImageAndHighlightRects(
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight],
      drawHighlights: Boolean
  ): Task[(BufferedImage, Seq[Rectangle])] = {
    for {
      luceneDoc <- ZIO.attempt {
        Using(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher
            .getByDocRef(docRef)
            .getOrElse(throw new DocumentNotFoundInIndex(docRef))
        }.get
      }
      rows <- searchRepo.getRowsByStartAndEndOffset(luceneDoc.rev, startOffset, endOffset)
      _ <- ZIO.attempt {
        rows match {
          case Nil =>
            throw new BadOffsetForImageSnippet(
              f"For document ${docRef.ref}, no rows found between start offset $startOffset and end offset $endOffset. Are you sure they're on the same page?"
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
      page <- searchRepo.getPage(rows.head.pageId)
      imageAndRectangles <- ZIO.attempt {
        val pageImagePath = docRef.getExistingPageImagePath(page.index)
        val originalImage = ImageIO.read(pageImagePath.toFile)

        val startRect = rows.head.rect
        val initialSnippetRect = rows.map(_.rect).tail.foldLeft(startRect)(_.union(_))

        val horizontalScale = originalImage.getWidth.toDouble / page.width.toDouble
        val verticalScale = originalImage.getHeight.toDouble / page.height.toDouble

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

        val highlightRectangles = allWordsToHighlight.map { highlightWord =>
          val highlightRect = Rectangle(
            left = (highlightWord.rect.left * horizontalScale).toInt,
            top = (highlightWord.rect.top * verticalScale).toInt,
            width = (highlightWord.rect.width * horizontalScale).toInt,
            height = (highlightWord.rect.height * verticalScale).toInt
          ).intersection(Rectangle(0, 0, originalImage.getWidth, originalImage.getHeight)).get

          val relativeRect = Rectangle(
            highlightRect.left - snippetRect.left - extra,
            highlightRect.top - snippetRect.top - extra,
            highlightRect.width + (extra * 2),
            highlightRect.height + (extra * 2)
          ).intersection(Rectangle(0, 0, snippetRect.width, snippetRect.height)).get

          relativeRect
        }

        if (log.isDebugEnabled) {
          log.debug(f"Rows: ${rows.mkString(", ")}")
          log.debug(f"Words to highlight: ${allWordsToHighlight.mkString(", ")}")
        }

        if (drawHighlights) {
          highlightRectangles.foreach { highlightRect =>
            graphics2D.setStroke(new BasicStroke(1))
            graphics2D.setPaint(Color.BLACK)
            graphics2D.drawRect(
              highlightRect.left,
              highlightRect.top,
              highlightRect.width,
              highlightRect.height
            )
            graphics2D.setColor(new Color(255, 255, 0, 127))
            graphics2D.fillRect(
              highlightRect.left,
              highlightRect.top,
              highlightRect.width,
              highlightRect.height
            )
          }
        }
        imageSnippet -> highlightRectangles
      }
    } yield { imageAndRectangles }
  }

  override def getIndexSize(): Task[Int] = ZIO.fromTry {
    Using(jochreIndex.searcherManager.acquire()) { searcher =>
      searcher.indexSize
    }
  }

  override def getTextAsHtml(docRef: DocReference): Task[String] = {
    for {
      docWithInfo <- ZIO.fromTry {
        Using(jochreIndex.searcherManager.acquire()) { searcher =>
          val document = searcher
            .getByDocRef(docRef)
            .getOrElse(
              throw new DocumentNotFoundInIndex(docRef)
            )
          val title = document.metadata.title
          val text = document.getText(IndexField.Text).getOrElse("")

          (document.rev, title, text)
        }
      }
      pages <- searchRepo.getPages(docWithInfo._1)
    } yield {
      val title = docWithInfo._2
      val text = docWithInfo._3

      val pagesWithText = pages
        .appended(DbPage(PageId(0), docWithInfo._1, 0, 0, 0, text.length))
        .foldLeft((Seq.empty[(Int, String)], 0, 0)) { case ((textSoFar, lastOffset, lastIndex), page) =>
          val nextPage = text.substring(lastOffset, page.offset).replaceAll("\n", "<br>")
          (textSoFar :+ (lastIndex -> nextPage), page.offset, page.index)
        }
        ._1

      val html = if (pagesWithText.isEmpty) {
        ""
      } else {
        pagesWithText.tail.map { case (pageIndex, text) =>
          f"""<div id="page$pageIndex">$text<hr></div>"""
        }.mkString
      }

      val response = title.map(t => f"<h1>$t</h1>").getOrElse("") + html
      response
    }

  }

  private def getWordGroup(wordsInRow: Seq[DbWord], wordOffset: Int): Seq[DbWord] = {
    val (wordsBefore, wordsAfter) = wordsInRow.span(_.startOffset < wordOffset)
    val word = wordsAfter.head
    val wordToStart = (wordsBefore :+ word).reverse
    val attachedWordsBefore = wordToStart
      .zip(wordToStart.tail)
      .takeWhile { case (word, prevWord) =>
        prevWord.endOffset == word.startOffset
      }
      .map(_._2)
      .reverse
    val attachedWordsAfter = wordsAfter
      .zip(wordsAfter.tail)
      .takeWhile { case (word, nextWord) =>
        word.endOffset == nextWord.startOffset
      }
      .map(_._2)

    val wordGroup = (attachedWordsBefore :+ word) ++ attachedWordsAfter
    wordGroup
  }

  override def getWordText(docRef: DocReference, wordOffset: Int): Task[String] = {
    for {
      luceneDoc <- ZIO.attempt {
        Using(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher
            .getByDocRef(docRef)
            .getOrElse(throw new DocumentNotFoundInIndex(docRef))
        }.get
      }
      wordsInRow <- searchRepo.getWordsInRow(luceneDoc.rev, wordOffset).mapAttempt { wordsInRow =>
        if (wordsInRow.isEmpty) {
          throw new WordOffsetNotFound(docRef, wordOffset)
        }
        wordsInRow
      }
      wordText <- ZIO.attempt {
        val wordGroup = getWordGroup(wordsInRow, wordOffset)
        val startOffset = wordGroup.map(_.startOffset).min
        val endOffset = wordGroup.map(_.endOffset).max

        Using(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher
            .getByDocRef(docRef)
            .getOrElse(throw new DocumentNotFoundInIndex(docRef))
            .getText(IndexField.Text)
            .map(_.substring(startOffset, endOffset))
            .getOrElse(throw new WordOffsetNotFound(docRef, wordOffset))
        }.get
      }
    } yield wordText
  }

  override def getWordImage(docRef: DocReference, wordOffset: Int): Task[BufferedImage] = {
    for {
      luceneDoc <- ZIO.attempt {
        Using(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher
            .getByDocRef(docRef)
            .getOrElse(throw new DocumentNotFoundInIndex(docRef))
        }.get
      }
      wordsInRow <- searchRepo.getWordsInRow(luceneDoc.rev, wordOffset).mapAttempt { wordsInRow =>
        if (wordsInRow.isEmpty) { throw new WordOffsetNotFound(docRef, wordOffset) }
        wordsInRow
      }
      row <- searchRepo.getRow(wordsInRow.head.rowId)
      page <- searchRepo.getPage(row.pageId)
      image <- ZIO.attempt {
        val pageImagePath = docRef.getExistingPageImagePath(page.index)

        val originalImage = ImageIO.read(pageImagePath.toFile)

        val wordGroup = getWordGroup(wordsInRow, wordOffset)

        val startRect = wordGroup.head.rect
        val wordRect = wordGroup.map(_.rect).tail.foldLeft(startRect)(_.union(_))

        val horizontalScale = originalImage.getWidth.toDouble / page.width.toDouble
        val verticalScale = originalImage.getHeight.toDouble / page.height.toDouble

        val snippetRect = Rectangle(
          left = (wordRect.left * horizontalScale).toInt - 5,
          top = (wordRect.top * verticalScale).toInt - 5,
          width = (wordRect.width * horizontalScale).toInt + 10,
          height = (wordRect.height * verticalScale).toInt + 10
        ).intersection(Rectangle(0, 0, originalImage.getWidth, originalImage.getHeight)).get

        val wordImage =
          new BufferedImage(snippetRect.width, snippetRect.height, BufferedImage.TYPE_INT_ARGB)

        val subImage = originalImage.getSubimage(
          snippetRect.left,
          snippetRect.top,
          snippetRect.width,
          snippetRect.height
        )
        val graphics2D = wordImage.createGraphics
        graphics2D.drawImage(subImage, 0, 0, subImage.getWidth, subImage.getHeight, null)

        wordImage
      }
    } yield { image }
  }

  override def suggestWord(
      username: String,
      ipAddress: Option[String],
      docRef: DocReference,
      wordOffset: Int,
      suggestion: String
  ): Task[Unit] = {
    for {
      luceneDoc <- ZIO.attempt {
        Using(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher
            .getByDocRef(docRef)
            .getOrElse(throw new DocumentNotFoundInIndex(docRef))
        }.get
      }
      wordsInRow <- searchRepo.getWordsInRow(luceneDoc.rev, wordOffset).mapAttempt { wordsInRow =>
        if (wordsInRow.isEmpty) { throw new WordOffsetNotFound(docRef, wordOffset) }
        wordsInRow
      }
      row <- searchRepo.getRow(wordsInRow.head.rowId)
      page <- searchRepo.getPage(row.pageId)
      rectAndText <- ZIO.attempt {
        val wordGroup = getWordGroup(wordsInRow, wordOffset)
        val startRect = wordGroup.head.rect
        val wordRect = wordGroup.map(_.rect).tail.foldLeft(startRect)(_.union(_))
        val horizontalScale = 10000 / page.width.toDouble
        val verticalScale = 10000 / page.height.toDouble
        val scaledRect = wordRect.copy(
          left = (wordRect.left * horizontalScale).toInt,
          top = (wordRect.top * verticalScale).toInt,
          width = (wordRect.width * horizontalScale).toInt,
          height = (wordRect.height * verticalScale).toInt
        )
        val startOffset = wordGroup.map(_.startOffset).min
        val endOffset = wordGroup.map(_.endOffset).max

        val previousText = Using(jochreIndex.searcherManager.acquire()) { searcher =>
          searcher
            .getByDocRef(docRef)
            .getOrElse(throw new DocumentNotFoundInIndex(docRef))
            .getText(IndexField.Text)
            .map(_.substring(startOffset, endOffset))
            .getOrElse(throw new WordOffsetNotFound(docRef, wordOffset))
        }.get

        (scaledRect, previousText)
      }
      _ <- suggestionRepo.insertSuggestion(
        username,
        ipAddress,
        docRef,
        page.index,
        rectAndText._1,
        suggestion,
        rectAndText._2,
        wordOffset
      )
    } yield ()
  }

  override def reindex(docRef: DocReference): Task[Int] = {
    log.info(f"About to re-index ${docRef.ref}")
    for {
      currentDoc <- searchRepo.getDocument(docRef)
      pageCount <- (for {
        altoStream <- ZIO.attempt {
          val altoFile = docRef.getAltoPath()
          new FileInputStream(altoFile.toFile)
        }
        alto <- readAndStoreAlto(docRef, altoStream)
        metadata <- ZIO.attempt {
          getMetadata(docRef).getOrElse(DocMetadata())
        }
        contentUpdated <- searchRepo.isContentUpdated(docRef)
        pageCount <- indexAlto(docRef, currentDoc.username, currentDoc.ipAddress, alto, metadata, contentUpdated)
        _ <- searchRepo.deleteOldRevs(docRef)
      } yield pageCount)
        .catchAll { ex: Throwable =>
          val sw = new StringWriter()
          val pw = new PrintWriter(sw)
          ex.printStackTrace(pw)
          val messageWithStackTrace = f"${ex.getMessage}\n${sw.toString}"
          (for {
            _ <- searchRepo.updateDocumentStatus(currentDoc.rev, DocumentStatus.Failed(messageWithStackTrace))
            _ <- searchRepo.unmarkForReindex(docRef)
          } yield ())
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
    } yield pageCount
  }

  private def getMetadata(docRef: DocReference): Option[DocMetadata] = {
    val metadataFile = docRef.getMetadataPath()
    val metadata = Option
      .when(metadataFile.toFile.exists()) {
        try {
          log.debug(f"Found metadata file at ${metadataFile.toFile.getPath}")
          val contents = Source.fromFile(metadataFile.toFile, StandardCharsets.UTF_8.name()).getLines().mkString("\n")
          val metadata = metadataReader.read(contents)

          Some(metadata)
        } catch {
          case t: Throwable =>
            log.error(f"Unable to read metadata file ${metadataFile.toFile.getPath}", t)
            None
        }
      }
      .flatten
    metadata
  }

  override def correctMetadata(
      username: String,
      ipAddress: Option[String],
      docRef: DocReference,
      field: MetadataField,
      newValue: String,
      applyEverywhere: Boolean
  ): Task[MetadataCorrectionId] = {
    log.info(f"Make metadata correction for doc ${docRef.ref}, field ${field.entryName}, value $newValue")
    val shouldSendMail = config.getBoolean("corrections.send-mail")
    for {
      oldValue <- ZIO.attempt {
        Using.resource(jochreIndex.searcherManager.acquire()) { searcher =>
          val luceneDoc = searcher
            .getByDocRef(docRef)
            .getOrElse(throw new DocumentNotFoundInIndex(docRef))

          // If the existing value is empty, we don't want to replace it everywhere, hence .filter(_.trim.nonEmpty)
          luceneDoc.getMetaValue(field).filter(_.trim.nonEmpty)
        }
      }
      docRefs <- ZIO
        .attempt {
          Using.resource(jochreIndex.searcherManager.acquire()) { searcher =>
            oldValue
              .flatMap(oldValue =>
                Option
                  .when(applyEverywhere) {
                    val searchQuery = SearchQuery(SearchCriterion.ValueIn(field.indexField, Seq(oldValue)))
                    searcher
                      .findMatchingRefs(searchQuery)
                  }
              )
              .getOrElse(Vector.empty) :+ docRef
          }
        }
        .mapAttempt(docRefs => docRefs.distinct.sortBy(_.ref))
      correctionId <- suggestionRepo.insertMetadataCorrection(
        username,
        ipAddress,
        field,
        oldValue,
        newValue,
        applyEverywhere,
        docRefs
      )
      correction <- suggestionRepo.getMetadataCorrection(correctionId)
      _ <- ZIO.attempt {
        if (shouldSendMail) {
          CorrectionMailer.mailCorrection(correction, docRefs)
        }
      }
      _ <-
        if (shouldSendMail) {
          suggestionRepo.markMetadataCorrectionAsSent(correctionId)
        } else {
          ZIO.succeed(())
        }
    } yield correctionId
  }

  override def undoMetadataCorrection(id: MetadataCorrectionId): Task[Seq[DocReference]] = {
    log.info(f"Undo metadata correction ${id.id}")
    for {
      ignoreCount <- suggestionRepo.ignoreMetadataCorrection(id)
      _ <- ZIO.attempt {
        if (ignoreCount == 0) {
          throw new UnknownMetadataCorrectionIdException(id)
        }
      }
      docRefs <- suggestionRepo.getMetadataCorrectionDocs(id)
    } yield docRefs
  }

  override def reindexWhereRequired(): Task[Boolean] = {
    val acquireTask = ZIO.attempt {
      val underway = reindexingUnderway.compareAndExchange(false, true)
      underway
    }

    def releaseTask(underway: Boolean) = ZIO.succeed {
      reindexingUnderway.compareAndExchange(true, false)
      if (!underway) {
        log.info("Finished reindex where required")
      }
    }

    def reindexTask(underway: Boolean) = {
      if (underway) {
        log.info("Re-indexing already underway.")
        ZIO.succeed(false)
      } else {
        for {
          _ <- ZIO.attempt {
            reindexingUnderway.set(true)
          }
          docRefs <- searchRepo.getDocumentsToReindex()
          _ <- ZIO.attempt {
            log.info(f"Reindex requested, found ${docRefs.size} documents to re-index")
          }
          _ <- ZStream
            .fromIterable(docRefs)
            .mapZIOParUnordered(indexParallelism) { docRef =>
              reindex(docRef)
                .catchAll { e: Throwable =>
                  ZIO.succeed(log.error(f"Unable to index ${docRef.ref}", e))
                }
            }
            .run(ZSink.foreach(pageCount => ZIO.succeed(log.info(f"Indexed $pageCount pages"))))
        } yield true
      }
    }

    ZIO.acquireReleaseWith(acquireTask)(releaseTask)(reindexTask)
  }

  override def getTerms(docRef: DocReference): Task[Map[String, Seq[IndexTerm]]] = {
    for {
      terms <- ZIO.attempt {
        val termLister = TermLister(jochreIndex.searcherManager)
        termLister.listTerms(docRef)
      }
    } yield terms
  }

  override def markForReindex(docRef: DocReference): Task[Unit] = {
    for {
      _ <- searchRepo.markForReindex(docRef)
    } yield ()
  }

  override def markAllForReindex(): Task[Unit] = for {
    _ <- searchRepo.markAllForReindex()
  } yield ()

  override def cleanUpAtStartUp(): Task[Unit] = for {
    result <- searchRepo.markUnderwayAsFailedAtStartup()
    _ <- ZIO.succeed(log.info(f"Marked $result underway documents as failed at startup."))
  } yield ()
}

object SearchService {
  lazy val live: ZLayer[JochreIndex & SearchRepo & SuggestionRepo & LanguageSpecificFilters, Nothing, SearchService] =
    ZLayer {
      for {
        index <- ZIO.service[JochreIndex]
        searchRepo <- ZIO.service[SearchRepo]
        suggestionRepo <- ZIO.service[SuggestionRepo]
        languageSpecificFilters <- ZIO.service[LanguageSpecificFilters]
      } yield SearchServiceImpl(
        index,
        searchRepo,
        suggestionRepo,
        languageSpecificFilters = Some(languageSpecificFilters)
      )
    }
}
