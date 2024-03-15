package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.api.HttpError
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import zio.ZIO

trait IndexLogic {
  def postIndexPdfLogic(
      token: ValidToken,
      pdfFileForm: PdfFileForm
  ): ZIO[Requirements, HttpError, IndexResponse] =
    ZIO.succeed(IndexHelper.indexResponseExample)
}
