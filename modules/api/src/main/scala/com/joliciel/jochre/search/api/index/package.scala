package com.joliciel.jochre.search.api

import sttp.model.Part

import java.io.File

package object index {
  case class PdfFileForm(
      pdfFile: Part[File],
      altoFile: Part[File],
      metadataFile: Part[File]
  )

  case class IndexResponse(
      pages: Int
  )

  object IndexHelper {
    val indexResponseExample: IndexResponse = IndexResponse(pages = 42)
  }
}
