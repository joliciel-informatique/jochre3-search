package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper, OkResponse}
import com.joliciel.jochre.search.core.{DocReference, MetadataField, UnknownMetadataFieldException}
import com.joliciel.jochre.search.core.service.{MetadataCorrectionId, SearchService}
import zio.ZIO

import java.io.FileInputStream

trait IndexLogic extends HttpErrorMapper {
  def putPdfLogic(
      token: ValidToken,
      pdfFileForm: PdfFileForm,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, IndexResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      pageCount <- searchService.addNewDocumentAsPdf(
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

  def putImageZipLogic(
      token: ValidToken,
      imageZipFileForm: ImageZipFileForm,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, IndexResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      pageCount <- searchService.addNewDocumentAsImages(
        imageZipFileForm.docReference,
        token.username,
        ipAddress,
        new FileInputStream(imageZipFileForm.imageZipFile.body),
        new FileInputStream(imageZipFileForm.altoFile.body),
        imageZipFileForm.metadataFile.map(m => new FileInputStream(m.body))
      )
      _ <- ZIO.attempt {
        imageZipFileForm.imageZipFile.body.delete()
        imageZipFileForm.altoFile.body.delete()
        imageZipFileForm.metadataFile.foreach(_.body.delete())
      }
    } yield IndexResponse(pageCount))
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to index image zip file", error))
      .mapError(mapToHttpError)

  def postAltoLogic(
      token: ValidToken,
      altoFileForm: AltoFileForm,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, IndexResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      pageCount <- searchService.updateAlto(
        altoFileForm.docReference,
        new FileInputStream(altoFileForm.altoFile.body)
      )
      _ <- ZIO.attempt {
        altoFileForm.altoFile.body.delete()
      }
    } yield IndexResponse(pageCount))
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to update alto for existing document", error))
      .mapError(mapToHttpError)

  def postMetadataLogic(
      token: ValidToken,
      metadataFileForm: MetadataFileForm,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, IndexResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      pageCount <- searchService.updateMetadata(
        metadataFileForm.docReference,
        new FileInputStream(metadataFileForm.metadataFile.body)
      )
      _ <- ZIO.attempt {
        metadataFileForm.metadataFile.body.delete()
      }
    } yield IndexResponse(pageCount))
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to update metadata for existing document", error))
      .mapError(mapToHttpError)

  def deleteDocumentLogic(
      token: ValidToken,
      docRef: DocReference,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, OkResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      pageCount <- searchService.removeDocument(
        docRef
      )
    } yield OkResponse())
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to remove document", error))
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
      _ <- searchService.reindexWhereRequired().forkDaemon
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
      _ <- searchService.correctMetadata(
        token.username,
        ipAddress,
        metadataCorrectionForm.docRef,
        metadataField,
        metadataCorrectionForm.value,
        metadataCorrectionForm.applyEverywhere
      )
      _ <- searchService.reindexWhereRequired().forkDaemon
    } yield OkResponse())
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to correct metadata", error))
      .mapError(mapToHttpError)

  def postUndoMetadataCorrectionLogic(
      token: ValidToken,
      metdataCorrectionId: MetadataCorrectionId
  ): ZIO[Requirements, HttpError, OkResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      _ <- searchService.undoMetadataCorrection(metdataCorrectionId)
      _ <- searchService.reindexWhereRequired().forkDaemon
    } yield OkResponse())
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to undo metadata correction", error))
      .mapError(mapToHttpError)

  def postReindexLogic(): ZIO[Requirements, HttpError, OkResponse] =
    for {
      searchService <- ZIO.service[SearchService]
      _ <- searchService.reindexWhereRequired().forkDaemon
    } yield OkResponse()

  def getTermsLogic(docRef: DocReference): ZIO[Requirements, HttpError, GetTermsResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      terms <- searchService.getTerms(docRef)
    } yield GetTermsResponse(terms))
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to get terms", error))
      .mapError(mapToHttpError)

  def postMarkForIndex(docRef: DocReference): ZIO[Requirements, HttpError, OkResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      _ <- searchService.markForReindex(docRef)
    } yield OkResponse())
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to mark for re-index", error))
      .mapError(mapToHttpError)

  def postMarkAllForIndex(): ZIO[Requirements, HttpError, OkResponse] =
    (for {
      searchService <- ZIO.service[SearchService]
      _ <- searchService.markAllForReindex()
    } yield OkResponse())
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to mark all for re-index", error))
      .mapError(mapToHttpError)
}
