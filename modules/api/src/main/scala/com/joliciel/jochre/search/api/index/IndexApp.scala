package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.api.HttpError.{BadRequest, Conflict, NotFound}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.{AuthenticationProvider, TokenAuthentication, ValidToken}
import com.joliciel.jochre.search.api.{HttpError, OkResponse, Roles}
import com.joliciel.jochre.search.core.service.MetadataCorrectionId
import com.joliciel.jochre.search.core.{CoreProtocol, DocReference, MetadataField}
import io.circe.generic.auto._
import sttp.capabilities.zio.ZioStreams
import sttp.model.StatusCode
import sttp.tapir.AnyEndpoint
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.ztapir._

import scala.concurrent.ExecutionContext

case class IndexApp(override val authenticationProvider: AuthenticationProvider, executionContext: ExecutionContext)
    extends TokenAuthentication
    with IndexLogic
    with IndexSchemaSupport
    with CoreProtocol {
  given ExecutionContext = executionContext

  private val putPdfEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (PdfFileForm, Option[String]),
    HttpError,
    IndexResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description(
            "Pdf file is not a valid pdf, or Alto file is not a zip file containing Alto XML"
          )
        )
      )
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[Conflict](
          StatusCode.Conflict,
          jsonBody[Conflict].description(
            "Document already exists in index"
          )
        )
      )
      .put
      .in("index")
      .in("pdf")
      .in(
        multipartBody[PdfFileForm]
      )
      .in(clientIp)
      .out(jsonBody[IndexResponse].example(IndexHelper.indexResponseExample))
      .description("Add a new document defined by a PDF, zipped Alto XML, and optionally metadata")

  private[index] val putPdfHttp =
    putPdfEndpoint.serverLogic[Requirements](token => input => putPdfLogic.tupled(Tuple1(token) ++ input))

  private val putImageZipEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (ImageZipFileForm, Option[String]),
    HttpError,
    IndexResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description(
            "Image zip file is not a valid zip file, or Alto file is not a zip file containing Alto XML"
          )
        )
      )
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[Conflict](
          StatusCode.Conflict,
          jsonBody[Conflict].description(
            "Document already exists in index"
          )
        )
      )
      .put
      .in("index")
      .in("image-zip")
      .in(
        multipartBody[ImageZipFileForm]
      )
      .in(clientIp)
      .out(jsonBody[IndexResponse].example(IndexHelper.indexResponseExample))
      .description(
        "Add a new document defined by a zip of image files, zipped Alto XML, and optionally metadata." +
          "Image files must be named [ref]_0001.png etc."
      )

  private val putImageZipHttp: ZServerEndpoint[Requirements, Any] =
    putImageZipEndpoint.serverLogic[Requirements](token => input => putImageZipLogic.tupled(Tuple1(token) ++ input))

  private val postAltoEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (AltoFileForm, Option[String]),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description(
            "Alto file is not a zip file containing Alto XML"
          )
        )
      )
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Document not found in index"
          )
        )
      )
      .post
      .in("index")
      .in("alto")
      .in(
        multipartBody[AltoFileForm]
      )
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description("Replace alto OCR layer for an existing document.")

  private val postAltoHttp: ZServerEndpoint[Requirements, Any] =
    postAltoEndpoint.serverLogic[Requirements](token => input => postAltoLogic.tupled(Tuple1(token) ++ input))

  private val postMetadataEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (MetadataFileForm, Option[String]),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description(
            "Metadata file is wrong format"
          )
        )
      )
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Document not found in index"
          )
        )
      )
      .post
      .in("index")
      .in("metadata")
      .in(
        multipartBody[MetadataFileForm]
      )
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description("Replace metadata for an existing document.")

  private val postMetadataHttp: ZServerEndpoint[Requirements, Any] =
    postMetadataEndpoint.serverLogic[Requirements](token => input => postMetadataLogic.tupled(Tuple1(token) ++ input))

  private val deleteDocumentEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (DocReference, Option[String]),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Document not found in index"
          )
        )
      )
      .delete
      .in("index")
      .in("document")
      .in(
        path[DocReference]("docRef")
      )
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description("Remove an existing document")

  private val deleteDocumentHttp: ZServerEndpoint[Requirements, Any] =
    deleteDocumentEndpoint.serverLogic[Requirements](token =>
      input => deleteDocumentLogic.tupled(Tuple1(token) ++ input)
    )

  private val postWordSuggestionEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (WordSuggestionForm, Option[String]),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint()
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Document reference not found"
          )
        )
      )
      .post
      .in("suggest-word")
      .in(
        jsonBody[WordSuggestionForm]
      )
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description("Make a suggestion for a given word that was OCR'd incorrectly.")

  private val postWordSuggestionHttp: ZServerEndpoint[Requirements, Any] =
    postWordSuggestionEndpoint.serverLogic[Requirements](token =>
      input => postWordSuggestionLogic.tupled(Tuple1(token) ++ input)
    )

  private val postMetadataCorrectionEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (MetadataCorrectionForm, Option[String]),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint()
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Document reference not found"
          )
        )
      )
      .post
      .in("correct-metadata")
      .in(
        jsonBody[MetadataCorrectionForm].example(
          MetadataCorrectionForm(
            DocReference("nybc200089"),
            MetadataField.Author.entryName,
            "שלום עליכם",
            applyEverywhere = false
          )
        )
      )
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description(
        f"Correct metadata for a given document reference. Field is one of ${MetadataField.values.map(_.entryName).mkString(", ")}"
      )

  private val postMetadataCorrectionHttp: ZServerEndpoint[Requirements, Any] =
    postMetadataCorrectionEndpoint.serverLogic[Requirements](token =>
      input => postMetadataCorrectionLogic.tupled(Tuple1(token) ++ input)
    )

  private val postReindexEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    Option[String],
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description(
            "Not sure when this could happen"
          )
        )
      )
      .post
      .in("reindex")
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description(
        f"Re-index all documents requiring re-indexing"
      )

  private val postReindexHttp: ZServerEndpoint[Requirements, Any] =
    postReindexEndpoint.serverLogic[Requirements](token => input => postReindexLogic(token, input))

  private val postUndoMetadataCorrectionEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        MetadataCorrectionId,
        Option[String]
    ),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Metadata correction not found for this id"
          )
        )
      )
      .post
      .in("undo-correction")
      .in(path[MetadataCorrectionId]("id"))
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description(
        f"Unto metadata correction for the id provided"
      )

  private val postUndoMetadataCorrectionHttp: ZServerEndpoint[Requirements, Any] =
    postUndoMetadataCorrectionEndpoint.serverLogic[Requirements](token =>
      input => postUndoMetadataCorrectionLogic.tupled(Tuple1(token) ++ input)
    )

  private val getTermsEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (DocReference, Option[Int], Option[Int]),
    HttpError,
    GetTermsResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Document not found in index"
          )
        )
      )
      .get
      .in("index")
      .in("document")
      .in(
        path[DocReference]("docRef")
      )
      .in("terms")
      .in(
        query[Option[Int]]("start-offset")
          .description("The start character offset of the first word with respect to the entire document (inclusive)")
          .example(Some(10200))
      )
      .in(
        query[Option[Int]]("end-offset")
          .description("The end character offset of the last word with respect to the entire document (exclusive)")
          .example(Some(10450))
      )
      .out(jsonBody[GetTermsResponse])
      .description("List terms for a document")

  private val getTermsHttp: ZServerEndpoint[Requirements, Any] =
    getTermsEndpoint.serverLogic[Requirements](_ => input => getTermsLogic.tupled(input))

  private val postMarkForReindexEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    (
        DocReference,
        Option[String]
    ),
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[NotFound](
          StatusCode.NotFound,
          jsonBody[NotFound].description(
            "Document not found in index"
          )
        )
      )
      .post
      .in("index")
      .in("document")
      .in(
        path[DocReference]("docRef")
      )
      .in("mark-for-reindex")
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description("Mark an existing document of re-indexing")

  private val postMarkForReindexHttp: ZServerEndpoint[Requirements, Any] =
    postMarkForReindexEndpoint.serverLogic[Requirements](token =>
      input => postMarkForIndex.tupled(Tuple1(token) ++ input)
    )

  private val postMarkAllForReindexEndpoint: ZPartialServerEndpoint[
    Requirements,
    String,
    ValidToken,
    Option[String],
    HttpError,
    OkResponse,
    Any
  ] =
    secureEndpoint(Roles.index)
      .errorOutVariantPrepend[HttpError](
        oneOfVariant[BadRequest](
          StatusCode.BadRequest,
          jsonBody[BadRequest].description(
            "Not sure when this could happen"
          )
        )
      )
      .post
      .in("index")
      .in("mark-all-for-reindex")
      .in(clientIp)
      .out(jsonBody[OkResponse].example(OkResponse()))
      .description(
        f"Mark all documents for re-index"
      )

  private val postMarkAllForReindexHttp: ZServerEndpoint[Requirements, Any] =
    postMarkAllForReindexEndpoint.serverLogic[Requirements](token => input => postMarkAllForIndex(token, input))

  val endpoints: List[AnyEndpoint] = List(
    putPdfEndpoint,
    putImageZipEndpoint,
    postAltoEndpoint,
    postMetadataEndpoint,
    deleteDocumentEndpoint,
    postWordSuggestionEndpoint,
    postMetadataCorrectionEndpoint,
    postUndoMetadataCorrectionEndpoint,
    postMarkForReindexEndpoint,
    postMarkAllForReindexEndpoint,
    postReindexEndpoint,
    getTermsEndpoint
  ).map(_.endpoint.tag("index"))

  val http: List[ZServerEndpoint[Requirements, Any & ZioStreams]] = List(
    putPdfHttp,
    putImageZipHttp,
    postAltoHttp,
    postMetadataHttp,
    deleteDocumentHttp,
    postWordSuggestionHttp,
    postMetadataCorrectionHttp,
    postUndoMetadataCorrectionHttp,
    postMarkForReindexHttp,
    postMarkAllForReindexHttp,
    postReindexHttp,
    getTermsHttp
  )
}
