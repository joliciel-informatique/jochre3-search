package com.joliciel.jochre.search.core

import com.joliciel.jochre.ocr.core.graphics.Rectangle

import java.io.StringReader
import java.time.Instant
import scala.util.Using
import scala.xml.XML

package object service {
  case class SearchResponse(results: Seq[SearchResult], totalCount: Long)

  case class SearchResult(
      docRef: DocReference,
      docRev: DocRev,
      metadata: DocMetadata,
      score: Double,
      snippets: Seq[Snippet]
  )

  case class Snippet(
      text: String,
      page: Int,
      start: Int,
      end: Int,
      highlights: Seq[Highlight]
  )

  case class Highlight(start: Int, end: Int)

  object SearchHelper {
    val searchResponseExample: SearchResponse = SearchResponse(
      results = Seq(
        SearchResult(
          docRef = DocReference("nybc200089"),
          docRev = DocRev(42),
          metadata = DocMetadata(
            title = Some("אלע ווערק"),
            titleEnglish = Some("Ale verk"),
            author = Some("שלום עליכם"),
            authorEnglish = Some("Sholem Aleykhem"),
            volume = Some("18"),
            publisher = Some("Nyu York : Sholem-Aleykhem folksfond"),
            publicationYear = Some("1917")
          ),
          score = 0.90,
          snippets = Seq(
            Snippet(
              text = "אין דער <b>אַלטער הײם</b>.",
              page = 11,
              start = 100,
              end = 118,
              highlights = Seq(Highlight(108, 117))
            )
          )
        )
      ),
      totalCount = 42
    )

    val aggregationBinsExample: AggregationBins = AggregationBins(
      Seq(
        AggregationBin("שלום עליכם", 212),
        AggregationBin("שלום אַש", 12)
      )
    )
  }

  private[core] case class DocRev(rev: Long) extends AnyVal

  private[service] case class DbDocument(rev: DocRev, ref: DocReference, created: Instant)

  private[service] case class PageId(id: Long) extends AnyVal
  private[service] case class DbPage(id: PageId, docRev: DocRev, index: Int, width: Int, height: Int, offset: Int)

  private[service] case class RowId(id: Long) extends AnyVal
  private[service] case class DbRow(
      id: RowId,
      pageId: PageId,
      index: Int,
      left: Int,
      top: Int,
      width: Int,
      height: Int
  ) {
    val rect = Rectangle(left, top, width, height)
  }

  private[service] case class WordId(id: Long) extends AnyVal
  private[service] case class DbWord(
      id: WordId,
      docRev: DocRev,
      rowId: RowId,
      startOffset: Int,
      endOffset: Int,
      hyphenatedOffset: Option[Int],
      left: Int,
      top: Int,
      width: Int,
      height: Int
  ) {
    val rect = Rectangle(left, top, width, height)
  }

  private[service] case class QueryId(id: Long) extends AnyVal
  private[service] case class DbQuery(
      id: QueryId,
      username: String,
      executed: Instant,
      criteria: SearchCriterion,
      query: Option[String],
      sort: Sort,
      first: Int,
      max: Int,
      resultCount: Int
  )

  private[service] case class WordSuggestionId(id: Long) extends AnyVal
  private[service] case class DbWordSuggestion(
      id: WordSuggestionId,
      username: String,
      created: Instant,
      docRef: DocReference,
      pageIndex: Int,
      left: Int,
      top: Int,
      width: Int,
      height: Int,
      suggestion: String,
      previousText: String,
      ignore: Boolean
  ) {
    val rect = Rectangle(left, top, width, height)
  }

  private[service] case class MetadataCorrectionId(id: Long) extends AnyVal
  private[service] case class DbMetadataCorrection(
      id: MetadataCorrectionId,
      username: String,
      created: Instant,
      field: MetadataField,
      oldValue: Option[String],
      newValue: String,
      applyEverywhere: Boolean,
      ignore: Boolean,
      sent: Boolean
  )

  trait MetadataReader {
    def read(fileContents: String): DocMetadata
  }

  object MetadataReader {
    val default: MetadataReader = (fileContents: String) => {
      import com.joliciel.jochre.ocr.core.utils.XmlImplicits._

      Using(new StringReader(fileContents)) { reader =>
        val fileXml = XML.load(reader)
        val title = (fileXml \\ "title-alt-script").headOption.map(_.textContent)
        val titleEnglish = (fileXml \\ "title").headOption.map(_.textContent)
        val author = (fileXml \\ "creator-alt-script").headOption.map(_.textContent)
        val authorEnglish = (fileXml \\ "creator").headOption.map(_.textContent)
        val date = (fileXml \\ "date").headOption.map(_.textContent)
        val publisher = (fileXml \\ "publisher").headOption.map(_.textContent)
        val volume = (fileXml \\ "volume").headOption.map(_.textContent)
        val url = (fileXml \\ "identifier-access").headOption.map(_.textContent)
        DocMetadata(
          title = title,
          titleEnglish = titleEnglish,
          author = author,
          authorEnglish = authorEnglish,
          publicationYear = date,
          publisher = publisher,
          volume = volume,
          url = url
        )
      }.get
    }
  }
}
