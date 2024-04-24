package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper, OkResponse}
import com.joliciel.jochre.search.core.{MetadataField, UnknownMetadataFieldException}
import com.joliciel.jochre.search.core.service.SearchService
import zio.ZIO

import java.io.FileInputStream

trait IndexLogic extends HttpErrorMapper {
  def postIndexPdfLogic(
      token: ValidToken,
      pdfFileForm: PdfFileForm,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, IndexResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      pageCount <- searchService.indexPdf(
        pdfFileForm.docReference,
        token.username,
        ipAddress,
        new FileInputStream(pdfFileForm.pdfFile.body),
        new FileInputStream(pdfFileForm.altoFile.body),
        pdfFileForm.metadataFile.map(m => new FileInputStream(m.body))
      )
      _ <- ZIO.attempt {
        pdfFileForm.pdfFile.body.delete()
        pdfFileForm.altoFile.body.delete()
        pdfFileForm.metadataFile.foreach(_.body.delete())
      }
    } yield IndexResponse(pageCount))
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to index pdf file", error))
      .mapError(mapToHttpError)

  def postWordSuggestionLogic(
      token: ValidToken,
      wordSuggestionForm: WordSuggestionForm,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, OkResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      _ <- searchService.suggestWord(
        token.username,
        ipAddress,
        wordSuggestionForm.docRef,
        wordSuggestionForm.offset,
        wordSuggestionForm.suggestion
      )
      _ <- searchService.reindex(wordSuggestionForm.docRef).forkDaemon
    } yield OkResponse())
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to make suggestion", error))
      .mapError(mapToHttpError)

  def postMetadataCorrectionLogic(
      token: ValidToken,
      metadataCorrectionForm: MetadataCorrectionForm,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, OkResponse] =
    (for {
      metadataField <- ZIO.attempt(
        MetadataField
          .withNameOption(metadataCorrectionForm.field)
          .getOrElse(throw new UnknownMetadataFieldException(metadataCorrectionForm.field))
      )
      searchService <- ZIO.service[SearchService]
      docRefs <- searchService.correctMetadata(
        token.username,
        ipAddress,
        metadataCorrectionForm.docRef,
        metadataField,
        metadataCorrectionForm.value,
        metadataCorrectionForm.applyEverywhere
      )
      _ <- ZIO.foreach(docRefs)(docRef => searchService.reindex(docRef)).forkDaemon
    } yield OkResponse())
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to correct metadata", error))
      .mapError(mapToHttpError)
}
