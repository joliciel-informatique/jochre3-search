package com.joliciel.jochre.search.core

import com.joliciel.jochre.ocr.core.graphics.Rectangle

import java.io.StringReader
import java.time.Instant
import scala.util.Using
import scala.xml.XML

package object search {
  case class SearchResponse(results: Seq[SearchResult], totalCount: Long)

  case class SearchResult(
      docRef: DocReference,
      score: Double,
      snippets: Seq[Snippet]
  )

  case class Snippet(
      text: String,
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
          score = 0.90,
          snippets = Seq(
            Snippet(
              text = "אין דער <b>אַלטער הײם</b>.",
              start = 100,
              end = 118,
              highlights = Seq(Highlight(108, 117))
            )
          )
        )
      ),
      totalCount = 42
    )
  }

  private[core] case class DocRev(rev: Long) extends AnyVal

  private[search] case class DbDocument(rev: DocRev, ref: DocReference, created: Instant)

  private[search] case class PageId(id: Long) extends AnyVal
  private[search] case class DbPage(id: PageId, docRev: DocRev, index: Int, width: Int, height: Int)

  private[search] case class RowId(id: Long) extends AnyVal
  private[search] case class DbRow(
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

  private[search] case class WordId(id: Long) extends AnyVal
  private[search] case class DbWord(
      id: WordId,
      docRev: DocRev,
      rowId: RowId,
      offset: Int,
      hyphenatedOffset: Option[Int],
      left: Int,
      top: Int,
      width: Int,
      height: Int
  ) {
    val rect = Rectangle(left, top, width, height)
  }

  trait MetadataReader {
    def read(fileContents: String): DocMetadata
  }

  object MetadataReader {
    val default: MetadataReader = (fileContents: String) => {
      import com.joliciel.jochre.ocr.core.utils.XmlImplicits._

      Using(new StringReader(fileContents)) { reader =>
        val fileXml = XML.load(reader)
        val title = (fileXml \\ "title-alt-script").headOption.map(_.textContent).getOrElse("")
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
          date = date,
          publisher = publisher,
          volume = volume,
          url = url
        )
      }.get
    }
  }
}
