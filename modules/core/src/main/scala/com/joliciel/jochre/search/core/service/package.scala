package com.joliciel.jochre.search.core

import com.joliciel.jochre.ocr.core.graphics.Rectangle

import java.io.StringReader
import java.time.Instant
import scala.util.Using
import scala.xml.{PrettyPrinter, XML}
import enumeratum.{DoobieEnum, Enum, EnumEntry}

package object service {
  case class SearchResponse(results: Seq[SearchResult], totalCount: Long)

  case class SearchResult(
      docRef: DocReference,
      docRev: DocRev,
      metadata: DocMetadata,
      ocrSoftware: Option[String] = None,
      score: Double,
      snippets: Seq[Snippet]
  )

  case class Snippet(
      text: String,
      page: Int,
      start: Int,
      end: Int,
      highlights: Seq[Highlight],
      deepLink: Option[String]
  )

  case class Highlight(start: Int, end: Int)

  case class HighlightedPage(
      physicalPageNumber: Int,
      startOffset: Int,
      text: String,
      highlights: Seq[Highlight],
      logicalPageNumber: Option[Int]
  )

  case class HighlightedDocument(
      title: String,
      pages: Seq[HighlightedPage]
  )

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
          ocrSoftware = Some("Jochre 3.0.0"),
          score = 0.90,
          snippets = Seq(
            Snippet(
              text = "אין דער <b>אַלטער הײם</b>.",
              page = 11,
              start = 100,
              end = 118,
              highlights = Seq(Highlight(108, 117)),
              deepLink = Some("https://archive.org/details/nybc200089/page/n10/mode/1up")
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

    val highlightedDocExample: HighlightedDocument =
      HighlightedDocument(
        title = "מאָטל, פּײסי דעם חזנס",
        pages = Seq(
          HighlightedPage(
            physicalPageNumber = 1,
            startOffset = 11,
            text = "הײנט איז יום־טוב — מע טאָר נישט װײנען !\n\n" +
              "א.\n\n" +
              "איך געה מיט אַײך אין נעװעט אױף װיפיעל דיהר װילט, אַז\n" +
              "קײנער אין דער װעלט איז נישט געװען אַזױ צופריעדען מיט’ן\n" +
              "װאַרעמען ליכטיגען נאָך־פּסח, װי איך, פּײסי דעם חונ’ס יונגעל,\n" +
              "מאָטעל, און דעם שכנ’ס קעלבעל, װאָס מע רופט דאָס „מעני“\n" +
              "(דאָס האָב איך, מאָטעל, דעם אַ נאָמען גענעבען „מעני“).\n" +
              "בײדע נלײך האָבען מיר דערפיהלט די ערשטע שטראַהלען\n" +
              "פון דער ואַרעמער זון אינ’ם ערשטען װאַרעמען נאָך־פּסח’דינען\n" +
              "מאָג, בײדע נלײך האָבען מיר דערשמעקט דעם ריח פונ’ם ערשטען\n" +
              "גרינעם גרעזעלע, װאָס קריכט, שפּראָצט־אַרױס פון דער נאָר־װאָס\n" +
              "אָפּגעדעקטער ערד, און בײדע גלײך זענען מיר אַרױסנעקראָכען\n" +
              "פונ’ם פינסטערען עננשאַפט צו מקבל־פּנים זײן דעם ערשטען\n" +
              "זיסען ליכטיגען װאַרעמען פריהלינגס־מאָרגען. איך, פּײסי דעם\n" +
              "חזנ’ס יונגעל, מאָטעל, בין אַרױס פון אַ טװאַן, פון אַ קאַלטען\n" +
              "נאַסליכען קעלער, װאָס שמעקט מיט זױערטײג און מיט רפואות\n" +
              "פון דער אַפּטײק ; און מעני, דעם שכנ’ס קעלבעל, האָט מען\n" +
              "אַרױסנעלאָזט נאָך פון אַײן ערגערען עפּוש : פון אַ קלײן, פינ־\n\n" +
              "9\n\n",
            highlights = Seq(Highlight(236, 242), Highlight(873, 879)),
            logicalPageNumber = Some(9)
          ),
          HighlightedPage(
            physicalPageNumber = 2,
            startOffset = 966,
            text = "10\n\n" +
              "שלום־עליכם\n\n" +
              "סטער, בלאָטיג, פאַרפּאַסקודיגט שטעלעכעל מיט אױסנעקרימטע\n" +
              "צעפליקטע װענט, װאָס װינטער בלאָזט־אַרײן אַהין דער שנײ און\n" +
              "זומער שמײסט אַרײן דער רעגען.\n" +
              "אַרױסנעחאַפּט זיך אױף גאָט’ס פרײע ליכטינע װעלט, האָבען\n" +
              "מיר בײדע, איך און מעני, אױס דאַנקבאַרקײט צו דער נאַטור,\n" +
              "זיך גענומען אױסדריקען אונזער צופריעדענהײט. איך, פּײסי דעם\n" +
              "חזנ’ס יונגעל, האָב אױפגעהױבען בײדע הענט אַרױף, אױפגעעפענט\n" +
              "דאָס מױל און אַרײנגעצױגען אין זיך די פרישע װאַרעמע לופט\n" +
              "אַזױ פיעל, װיפיעל איך האָב געקאָנט, — און עס האָט זיך מיר\n" +
              "אױסגעדאַכט, אַז איך װאַכס אין דער הױך, און עס ציהט מיך\n" +
              "אַרױף־אַרױף, אין דער טיעפער־טיעפער בלױער יאַרמעלקע אַרײן,\n" +
              "דאָרט װאו די שיטערע רױכיגע װאָלקענס שװעבען, דאָרט װאו\n" +
              "די װײסע פױנלען טוקען זיך, באַװײזען זיך און װערען פאַרשװאונ־\n" +
              "דען מיט אַ קװיטש און מיט אַ צװיטשער, און עס רײסט זיך\n" +
              "אַרױס פון מײן אָנגעפילטער ברוסט, אָהן מײן װיסען, אַ מין גע־\n" +
              "זאַנג, נאָך שענער װי יום־טוב מיט’ן טאַטען בײ’ם עמוד, אַ נער\n" +
              "זאַנג אָהן װערטער, אָהן נאָטען, אָהן אַ שום מאָטיװ, אַ מין נאַטור־\n" +
              "געזאַנג פון אַ װאַסער־פאַל, פון יאָגענדיגע חװאַליעס, אַ מין שיר—\n" +
              "השירים, אַ געטליכע התפּעלות, אַ הימלישע באגײסטערונג\n" +
              "אױ־װעה, טאַטע ! אױ־װעה, פאָטער ! אױ־װעה, לעבעדיגער\n" +
              "נאָאָאָאָאָט !!!\n" +
              "אָט אַזױ האָט אַרױסבאַװיזען זײן צופריעדענהײט מיט’ן ער־\n" +
              "שטען פריהלינגס־טאָג פּײסי דעם חזנ’ס יונגעל. גאַנץ אַנדערש\n" +
              "האָט דאָס אױסגעדריקט מעני, דעם שכנ’ס קעלבעל.\n" +
              "מעני, דעם שכנ’ס קעלבעל, האָט קודם אַרײנגעשטעקט די\n" +
              "שװאַרצע נאַסע מאָרדע אין מיסט אַרײן, אַ שאַר געטהאָן מיט’ן\n" +
              "פאָדערשטען פיסעל די ערד אונטער זיך אַמאָל דרײ, פאַרריסען\n" +
              "דעם עק, אױפגעשפּרוננען נאָכדעם אױף אַלע פיער, און אַרױס־\n" +
              "געלאָזט אַ טומפּיגען „מע !“ דער „מע“ האָט מיר אױסגעװיזען\n\n",
            highlights = Seq(
              Highlight(1302, 1308),
              Highlight(1326, 1332)
            ),
            logicalPageNumber = Some(10)
          )
        )
      )
  }

  sealed trait DocumentStatusCode extends EnumEntry

  object DocumentStatusCode extends Enum[DocumentStatusCode] with DoobieEnum[DocumentStatusCode] {
    val values = findValues

    case object Underway extends DocumentStatusCode
    case object Complete extends DocumentStatusCode
    case object Indexed extends DocumentStatusCode
    case object Failed extends DocumentStatusCode

    def toEnum(code: DocumentStatusCode): String = code.entryName
    def fromEnum(s: String): Option[DocumentStatusCode] = DocumentStatusCode.withNameOption(s)
  }

  sealed trait DocumentStatus {
    def code: DocumentStatusCode
  }

  object DocumentStatus {
    case object Underway extends DocumentStatus {
      val code: DocumentStatusCode = DocumentStatusCode.Underway
    }
    case object Complete extends DocumentStatus {
      val code: DocumentStatusCode = DocumentStatusCode.Complete
    }
    case object Indexed extends DocumentStatus {
      val code: DocumentStatusCode = DocumentStatusCode.Indexed
    }
    case class Failed(reason: String) extends DocumentStatus {
      val code: DocumentStatusCode = DocumentStatusCode.Failed
    }
  }

  case class DocRev(rev: Long) extends AnyVal

  private[service] case class DbDocument(
      rev: DocRev,
      ref: DocReference,
      username: String,
      ipAddress: Option[String],
      created: Instant,
      statusCode: DocumentStatusCode,
      failureReason: Option[String],
      statusUpdated: Instant
  ) {
    val status: DocumentStatus = statusCode match {
      case DocumentStatusCode.Underway => DocumentStatus.Underway
      case DocumentStatusCode.Complete => DocumentStatus.Complete
      case DocumentStatusCode.Indexed  => DocumentStatus.Indexed
      case DocumentStatusCode.Failed   => DocumentStatus.Failed(failureReason.getOrElse("Unknown"))
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

  case class WordSuggestionId(id: Long) extends AnyVal
  case class WordSuggestionRev(rev: Long) extends AnyVal

  object WordSuggestionRev {
    val ordering = new Ordering[WordSuggestionRev] {
      override def compare(x: WordSuggestionRev, y: WordSuggestionRev): Int = x.rev.compareTo(y.rev)
    }
  }
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
      ignore: Boolean,
      offset: Int,
      rev: WordSuggestionRev
  ) {
    val rect = Rectangle(left, top, width, height)
  }

  case class MetadataCorrectionId(id: Long) extends AnyVal
  private[service] case class MetadataCorrectionRev(rev: Long) extends AnyVal
  object MetadataCorrectionRev {
    val ordering = new Ordering[MetadataCorrectionRev] {
      override def compare(x: MetadataCorrectionRev, y: MetadataCorrectionRev): Int = x.rev.compareTo(y.rev)
    }
  }
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
      sent: Boolean,
      rev: MetadataCorrectionRev
  )

  private[service] case class DbIndexedDocument(
      docRef: DocReference,
      docRev: DocRev,
      wordSuggestionRev: Option[WordSuggestionRev],
      reindex: Boolean,
      indexTime: Instant
  )

  private[service] case class DbIndexedDocumentCorrection(
      docRef: DocReference,
      field: MetadataField,
      rev: MetadataCorrectionRev
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
          val collections = (fileXml \\ "collection").map(_.textContent)

          DocMetadata(
            title = title,
            titleEnglish = titleEnglish,
            author = author,
            authorEnglish = authorEnglish,
            publicationYear = date,
            publisher = publisher,
            volume = volume,
            url = url,
            collections = collections
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
          {metadata.collections.map(value => <collection>{value}</collection>)}
        </metadata>

        val prettyPrinter = new PrettyPrinter(120, 2)
        prettyPrinter.format(xml)
      }
    }
  }
}
