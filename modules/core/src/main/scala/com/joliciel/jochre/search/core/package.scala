package com.joliciel.jochre.search

import com.typesafe.config.ConfigFactory
import enumeratum.{Enum, EnumEntry}

import java.nio.file.Path

package object core {
  private val config = ConfigFactory.load().getConfig("jochre.search.index")
  private val contentDir = Path.of(config.getString("content-directory"))

  sealed trait IndexField extends EnumEntry {
    def isMultiValue: Boolean = false
    def aggregatable: Boolean = false
  }

  object IndexField extends Enum[IndexField] {
    val values: IndexedSeq[IndexField] = findValues

    val aggregatableFields: Seq[IndexField] = values.filter(_.aggregatable)

    case object Reference extends IndexField

    case object Revision extends IndexField

    case object Author extends IndexField {
      override def aggregatable: Boolean = true
    }

    case object Title extends IndexField

    case object Text extends IndexField

    case object IndexTime extends IndexField
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
  }

  case class DocMetadata(
      title: String,
      author: Option[String] = None,
      titleEnglish: Option[String] = None,
      authorEnglish: Option[String] = None,
      date: Option[String] = None,
      publisher: Option[String] = None,
      volume: Option[String] = None,
      url: Option[String] = None
  )

  case class AggregationBin(label: String, count: Int)

  case class AggregationBins(bins: Seq[AggregationBin])
}
