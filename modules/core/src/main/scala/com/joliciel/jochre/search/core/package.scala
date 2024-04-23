package com.joliciel.jochre.search

import com.typesafe.config.ConfigFactory
import enumeratum.{DoobieEnum, Enum, EnumEntry}

import java.nio.file.Path

package object core {
  private val config = ConfigFactory.load().getConfig("jochre.search.index")
  private val contentDir = Path.of(config.getString("content-directory"))

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
  }

  case class DocReference(ref: String) {
    def getBookDir(): Path = {
      contentDir.resolve(ref)
    }

    def getPageImagePath(pageNumber: Int): Path = {
      val bookDir = this.getBookDir()
      val imageFileName = f"${ref}_$pageNumber%04d.png"
      bookDir.resolve(imageFileName)
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
      url: Option[String] = None
  )

  case class AggregationBin(label: String, count: Int)

  case class AggregationBins(bins: Seq[AggregationBin])

  sealed trait Sort

  object Sort {
    case object Score extends Sort

    case class Field(field: IndexField, ascending: Boolean) extends Sort
  }
}
