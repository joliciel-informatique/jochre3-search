package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.lucene.IndexTerm
import com.joliciel.jochre.search.core.{DocReference, MetadataField}
import sttp.model.Part

import java.io.File

package object index {
  case class PdfFileForm(
      docReference: DocReference,
      pdfFile: Part[File],
      altoFile: Part[File],
      metadataFile: Option[Part[File]]
  )

  case class ImageZipFileForm(
      docReference: DocReference,
      imageZipFile: Part[File],
      altoFile: Part[File],
      metadataFile: Option[Part[File]]
  )

  case class AltoFileForm(
      docReference: DocReference,
      altoFile: Part[File]
  )

  case class MetadataFileForm(
      docReference: DocReference,
      metadataFile: Part[File]
  )

  case class IndexResponse(
      pages: Int
  )

  case class WordSuggestionForm(
      docRef: DocReference,
      offset: Int,
      suggestion: String
  )

  case class MetadataCorrectionForm(
      docRef: DocReference,
      field: String,
      value: String,
      applyEverywhere: Boolean
  )

  case class GetTermsResponse(
      terms: Map[String, Seq[IndexTerm]]
  )

  object IndexHelper {
    val indexResponseExample: IndexResponse = IndexResponse(pages = 42)
  }
}
