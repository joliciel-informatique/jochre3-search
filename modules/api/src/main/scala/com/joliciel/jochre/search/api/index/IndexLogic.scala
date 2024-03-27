package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import com.joliciel.jochre.search.core.search.SearchService
import zio.ZIO

import java.io.FileInputStream

trait IndexLogic extends HttpErrorMapper {
  def postIndexPdfLogic(
      token: ValidToken,
      pdfFileForm: PdfFileForm
  ): ZIO[Requirements, HttpError, IndexResponse] = {
    (for {
      searchService <- ZIO.service[SearchService]
      pageCount <- searchService.indexPdf(
        pdfFileForm.docReference,
        new FileInputStream(pdfFileForm.pdfFile.body),
        new FileInputStream(pdfFileForm.altoFile.body),
        pdfFileForm.metadataFile.map(m => new FileInputStream(m.body))
      )
    } yield IndexResponse(pageCount))
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to index pdf file", error))
      .mapError(mapToHttpError)
  }
}
