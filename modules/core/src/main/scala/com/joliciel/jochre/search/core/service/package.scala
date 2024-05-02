package com.joliciel.jochre.search.core

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.search.core.service.IndexStatusCode.values
import enumeratum.{DoobieEnum, Enum, EnumEntry}

import java.io.StringReader
import java.time.Instant
import scala.util.Using
import scala.xml.{PrettyPrinter, XML}

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

  sealed trait IndexStatusCode extends EnumEntry

  object IndexStatusCode extends Enum[IndexStatusCode] with DoobieEnum[IndexStatusCode] {
    val values = findValues

    case object Unindexed extends IndexStatusCode
    case object NewSuggestion extends IndexStatusCode
    case object NewMetadata extends IndexStatusCode
    case object Indexed extends IndexStatusCode

    def toEnum(code: IndexStatusCode): String = code.entryName

    def fromEnum(s: String): Option[IndexStatusCode] = IndexStatusCode.withNameOption(s)

    object MyOrdering extends Ordering[IndexStatusCode] {
      def compare(x: IndexStatusCode, y: IndexStatusCode): Int =
        (x, y) match {
          // assuming that the ordering is Unindexed < NewSuggestion < NewMetadata < Indexed...
          case (_, _) if (x eq y) => 0
          case (Unindexed, _) => -1
          case (_, Unindexed) => 1
          case (NewSuggestion, _) => -1
          case (_, NewSuggestion) => 1
          case (NewMetadata, _) => -1
          case (_, NewMetadata) => 1
          case _ => 0 // (Indexed, Indexed)
        }
    }
  }

  sealed trait IndexStatus {
    def code: IndexStatusCode
    val newSuggestionOffset: Option[Int] = None
  }

  object IndexStatus {
    case object Unindexed extends IndexStatus {
      override val code = IndexStatusCode.Unindexed
    }
    case class NewSuggestion(offset: Int) extends IndexStatus {
      override val code = IndexStatusCode.NewSuggestion
      override val newSuggestionOffset: Option[Int] = Some(offset)
    }
    case object NewMetadata extends IndexStatus {
      override val code = IndexStatusCode.NewMetadata
    }
    case object Indexed extends IndexStatus {
      override val code = IndexStatusCode.Indexed
    }
  }

  private[core] case class DocRev(rev: Long) extends AnyVal

  private[service] case class DbDocument(
      rev: DocRev,
      ref: DocReference,
      username: String,
      ipAddress: Option[String],
      created: Instant,
      indexStatusCode: IndexStatusCode,
      newSuggestionOffset: Option[Int]
  ) {
    lazy val indexStatus: IndexStatus = (indexStatusCode, newSuggestionOffset) match {
      case (IndexStatusCode.Unindexed, None)             => IndexStatus.Unindexed
      case (IndexStatusCode.NewSuggestion, Some(offset)) => IndexStatus.NewSuggestion(offset)
      case (IndexStatusCode.NewMetadata, None)           => IndexStatus.NewMetadata
      case (IndexStatusCode.Indexed, None)               => IndexStatus.Indexed
      case (code, offset) => throw new Exception(f"Impossible index status: ${code.entryName}, $offset")
    }
    def updateIndexStatusIfLessRestrictive(
        indexStatus: IndexStatus
    ): Option[DbDocument] = {
      val earlierSuggestion = (this.indexStatus, indexStatus) match {
        case (IndexStatus.NewSuggestion(myOffset), IndexStatus.NewSuggestion(yourOffset)) =>
          yourOffset < myOffset
        case _ => false
      }
      import IndexStatusCode.MyOrdering.mkOrderingOps
      Option.when(
        indexStatus.code < this.indexStatusCode || earlierSuggestion
      ) {
        this.copy(indexStatusCode = indexStatus.code, newSuggestionOffset = indexStatus.newSuggestionOffset)
      }
    }
  }

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
      ipAddress: Option[String],
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
      ipAddress: Option[String],
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

  case class MetadataCorrectionId(id: Long) extends AnyVal
  private[service] case class DbMetadataCorrection(
      id: MetadataCorrectionId,
      username: String,
      ipAddress: Option[String],
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

    def write(metadata: DocMetadata): String
  }

  object MetadataReader {
    val default: MetadataReader = new MetadataReader {

      override def read(fileContents: String): DocMetadata = {
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

      override def write(metadata: DocMetadata): String = {
        val xml = <metadata>
          {metadata.volume.map(value => <volume>{value}</volume>).orNull}
          {metadata.titleEnglish.map(value => <title>{value}</title>).orNull}
          {metadata.authorEnglish.map(value => <creator>{value}</creator>).orNull}
          {metadata.publisher.map(value => <publisher>{value}</publisher>).orNull}
          {metadata.publicationYear.map(value => <date>{value}</date>).orNull}
          {metadata.title.map(value => <title-alt-script>{value}</title-alt-script>).orNull}
          {metadata.author.map(value => <creator-alt-script>{value}</creator-alt-script>).orNull}
          {metadata.url.map(value => <identifier-access>{value}</identifier-access>).orNull}
        </metadata>

        val prettyPrinter = new PrettyPrinter(120, 2)
        prettyPrinter.format(xml)
      }
    }
  }
}
