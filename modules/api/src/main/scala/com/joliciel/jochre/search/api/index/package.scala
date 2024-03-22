package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.DocReference
import sttp.model.Part

import java.io.File

package object index {
  case class PdfFileForm(
      docReference: DocReference,
      pdfFile: Part[File],
      altoFile: Part[File],
      metadataFile: Option[Part[File]]
  )

  case class IndexResponse(
      pages: Int
  )

  object IndexHelper {
    val indexResponseExample: IndexResponse = IndexResponse(pages = 42)
  }
}
