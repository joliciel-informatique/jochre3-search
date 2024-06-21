package com.joliciel.jochre.search

import com.typesafe.config.ConfigFactory
import enumeratum.{DoobieEnum, Enum, EnumEntry}

import java.nio.file.Path
import scala.jdk.CollectionConverters._

package object core {
  private val config = ConfigFactory.load().getConfig("jochre.search")
  private val indexConfig = config.getConfig("index")
  private val contentDir = Path.of(indexConfig.getString("content-directory"))
  private val defaultBookUrl = Option.when(config.hasPath("default-book-url"))(config.getString("default-book-url"))
  private val bookUrlsByCollection = config
    .getConfigList("book-urls-by-collection")
    .asScala
    .map(config => config.getString("collection") -> config.getString("url"))
  private val defaultDeepLink = Option.when(config.hasPath("default-deep-link"))(config.getString("default-deep-link"))
  private val deepLinksByCollection = config
    .getConfigList("deep-links-by-collection")
    .asScala
    .map(config => config.getString("collection") -> config.getString("url"))

  sealed trait MetadataField extends EnumEntry {
    def indexField: IndexField
    def applyToMetadata(metadata: DocMetadata, value: String): DocMetadata
  }
  object MetadataField extends Enum[MetadataField] with DoobieEnum[MetadataField] {
    val values: IndexedSeq[MetadataField] = findValues

    case object Author extends MetadataField {
      override val indexField: IndexField = IndexField.Author

      override def applyToMetadata(metadata: DocMetadata, value: String): DocMetadata =
        metadata.copy(author = Some(value))
    }
    case object AuthorEnglish extends MetadataField {
      override val indexField: IndexField = IndexField.AuthorEnglish

      override def applyToMetadata(metadata: DocMetadata, value: String): DocMetadata =
        metadata.copy(authorEnglish = Some(value))
    }
    case object Title extends MetadataField {
      override val indexField: IndexField = IndexField.Title

      override def applyToMetadata(metadata: DocMetadata, value: String): DocMetadata =
        metadata.copy(title = Some(value))
    }
    case object TitleEnglish extends MetadataField {
      override val indexField: IndexField = IndexField.TitleEnglish

      override def applyToMetadata(metadata: DocMetadata, value: String): DocMetadata =
        metadata.copy(titleEnglish = Some(value))
    }
    case object Volume extends MetadataField {
      override val indexField: IndexField = IndexField.Volume

      override def applyToMetadata(metadata: DocMetadata, value: String): DocMetadata =
        metadata.copy(volume = Some(value))
    }
    case object Publisher extends MetadataField {
      override val indexField: IndexField = IndexField.Publisher

      override def applyToMetadata(metadata: DocMetadata, value: String): DocMetadata =
        metadata.copy(publisher = Some(value))
    }
    case object PublicationYear extends MetadataField {
      override val indexField: IndexField = IndexField.PublicationYear

      override def applyToMetadata(metadata: DocMetadata, value: String): DocMetadata =
        metadata.copy(publicationYear = Some(value))
    }
  }

  sealed trait FieldKind extends EnumEntry
  object FieldKind extends Enum[FieldKind] {
    val values: IndexedSeq[FieldKind] = findValues

    case object String extends FieldKind
    case object Integer extends FieldKind
    case object Text extends FieldKind
    case object Instant extends FieldKind
    case object MultiString extends FieldKind
  }

  sealed trait IndexField extends EnumEntry {
    def isTokenized: Boolean = kind == FieldKind.Text
    def isMultiValue: Boolean = false
    def aggregatable: Boolean = false
    def sortable: Boolean = false
    def kind: FieldKind
  }

  object IndexField extends Enum[IndexField] {
    val values: IndexedSeq[IndexField] = findValues

    val aggregatableFields: Seq[IndexField] = values.filter(_.aggregatable)

    case object Reference extends IndexField {
      override def kind: FieldKind = FieldKind.String

      override def sortable: Boolean = true
    }

    case object Revision extends IndexField {
      override def kind: FieldKind = FieldKind.String
    }

    case object Author extends IndexField {
      override def aggregatable: Boolean = true

      override def kind: FieldKind = FieldKind.String
    }

    case object AuthorEnglish extends IndexField {
      override def aggregatable: Boolean = true

      override def kind: FieldKind = FieldKind.String
    }

    case object Title extends IndexField {
      override def kind: FieldKind = FieldKind.Text
    }

    case object TitleEnglish extends IndexField {
      override def kind: FieldKind = FieldKind.Text
    }

    case object Volume extends IndexField {
      override def kind: FieldKind = FieldKind.String
    }

    case object Text extends IndexField {
      override def kind: FieldKind = FieldKind.Text
    }

    case object IndexTime extends IndexField {
      override def kind: FieldKind = FieldKind.Instant
    }

    case object Publisher extends IndexField {
      override def kind: FieldKind = FieldKind.String
    }

    case object PublicationYear extends IndexField {
      override def kind: FieldKind = FieldKind.String
    }

    case object PublicationYearAsNumber extends IndexField {
      override def kind: FieldKind = FieldKind.Integer
      override def sortable: Boolean = true
    }

    case object URL extends IndexField {
      override def kind: FieldKind = FieldKind.String
    }

    case object OCRSoftware extends IndexField {
      override def kind: FieldKind = FieldKind.String
    }

    case object Collection extends IndexField {
      override def kind: FieldKind = FieldKind.MultiString
    }
  }

  private val defaultExtension = "png"
  private val secondaryExtension = "jpg"

  case class DocReference(ref: String) {
    def getBookDir(): Path = {
      contentDir.resolve(ref)
    }

    def getPageImagePath(pageNumber: Int): Path = {
      val bookDir = this.getBookDir()
      val jpgFileName = f"${ref}_$pageNumber%04d.$defaultExtension"
      bookDir.resolve(jpgFileName)
    }

    def getExistingPageImagePath(pageNumber: Int): Path = {
      val bookDir = this.getBookDir()
      val defaultFileName = f"${ref}_$pageNumber%04d.$defaultExtension"
      val defaultImagePath = bookDir.resolve(defaultFileName)
      if (defaultImagePath.toFile.exists()) {
        defaultImagePath
      } else {
        val secondaryFileName = f"${ref}_$pageNumber%04d.$secondaryExtension"
        val secondaryImagePath = bookDir.resolve(secondaryFileName)
        if (secondaryImagePath.toFile.exists()) {
          secondaryImagePath
        } else {
          throw new ImageFileNotFoundException(this, pageNumber)
        }
      }
    }

    def getAltoPath(): Path = {
      val bookDir = this.getBookDir()
      val altoFileName = f"${ref}_alto4.zip"
      bookDir.resolve(altoFileName)
    }

    def getMetadataPath(): Path = {
      val bookDir = this.getBookDir()
      val metadataFileName = f"${ref}_meta.xml"
      bookDir.resolve(metadataFileName)
    }
  }

  case class DocMetadata(
      title: Option[String] = None,
      author: Option[String] = None,
      titleEnglish: Option[String] = None,
      authorEnglish: Option[String] = None,
      publicationYear: Option[String] = None,
      publisher: Option[String] = None,
      volume: Option[String] = None,
      url: Option[String] = None,
      collections: Seq[String] = Seq.empty
  ) {
    def getBookUrl(ref: DocReference): Option[String] = {
      val template = bookUrlsByCollection
        .find(b => collections.contains(b._1))
        .map(_._2)
        .orElse(defaultBookUrl)
        .orElse(url)

      template.map { template =>
        template
          .replaceAll(raw"$$\{REF\}", ref.ref)
      }
    }

    def getDeepLink(ref: DocReference, page: Int): Option[String] = {
      val template = deepLinksByCollection
        .find(b => collections.contains(b._1))
        .map(_._2)
        .orElse(defaultDeepLink)

      template.map { template =>
        template
          .replaceAll(raw"$$\{REF\}", ref.ref)
          .replaceAll(raw"$$\{PAGE\}", page.toString)
      }
    }
  }

  case class AggregationBin(label: String, count: Int)

  case class AggregationBins(bins: Seq[AggregationBin])

  sealed trait Sort

  object Sort {
    case object Score extends Sort

    case class Field(field: IndexField, ascending: Boolean) extends Sort
  }
}
