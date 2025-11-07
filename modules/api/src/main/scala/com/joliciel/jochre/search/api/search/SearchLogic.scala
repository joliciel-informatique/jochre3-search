package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper, UnknownSortException}
import com.joliciel.jochre.search.core.service.{Highlight, HighlightedDocument, SearchResponse, SearchService}
import com.joliciel.jochre.search.core.{
  AggregationBins,
  DocReference,
  IndexField,
  NoSearchCriteriaException,
  SearchCriterion,
  SearchQuery,
  UnknownIndexFieldException
}
import zio.stream.{ZPipeline, ZStream}
import zio.{Task, ZIO}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.Base64
import javax.imageio.ImageIO

trait SearchLogic extends HttpErrorMapper {
  private val defaultIp = "127.0.0.1"

  def getSearchLogicNoAuth(
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Option[Boolean],
      strict: Option[Boolean],
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      first: Int,
      max: Int,
      maxSnippets: Option[Int],
      rowPadding: Option[Int],
      sort: Option[String],
      physicalNewlines: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, SearchResponse] = getSearchLogicInternal(
    query,
    title,
    authors,
    authorInclude,
    strict,
    fromYear,
    toYear,
    docRefs,
    first,
    max,
    maxSnippets,
    rowPadding,
    sort,
    physicalNewlines,
    ipAddress,
    ipAddress.getOrElse(defaultIp)
  )

  def getSearchLogic(
      token: ValidToken,
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Option[Boolean],
      strict: Option[Boolean],
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      first: Int,
      max: Int,
      maxSnippets: Option[Int],
      rowPadding: Option[Int],
      sort: Option[String],
      physicalNewlines: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, SearchResponse] = getSearchLogicInternal(
    query,
    title,
    authors,
    authorInclude,
    strict,
    fromYear,
    toYear,
    docRefs,
    first,
    max,
    maxSnippets,
    rowPadding,
    sort,
    physicalNewlines,
    ipAddress,
    token.username
  )

  private def getSearchLogicInternal(
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Option[Boolean],
      strict: Option[Boolean],
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      first: Int,
      max: Int,
      maxSnippets: Option[Int],
      rowPadding: Option[Int],
      sort: Option[String],
      physicalNewlines: Option[Boolean],
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, SearchResponse] = {
    for {
      searchQuery <- getSearchQuery(
        query,
        title,
        authors,
        authorInclude,
        strict,
        fromYear,
        toYear,
        docRefs
      )
      searchService <- ZIO.service[SearchService]
      parsedSort <- ZIO.attempt {
        try {
          sort.map(SortKind.withName).getOrElse(SortKind.Score).toSort
        } catch {
          case nsee: NoSuchElementException => throw new UnknownSortException(nsee.getMessage)
        }
      }
      searchResponse <- searchService.search(
        searchQuery,
        parsedSort,
        first,
        max,
        maxSnippets,
        rowPadding,
        logUser,
        ipAddress,
        physicalNewLines = physicalNewlines.getOrElse(true)
      )
    } yield searchResponse
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to search", error))
    .mapError(mapToHttpError)

  def getImageSnippetLogic(
      token: ValidToken,
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    getImageSnippetLogicInternal(
      docRef,
      startOffset,
      endOffset,
      highlights,
      ipAddress,
      token.username
    )

  def getImageSnippetLogicNoAuth(
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    getImageSnippetLogicInternal(
      docRef,
      startOffset,
      endOffset,
      highlights,
      ipAddress,
      ipAddress.getOrElse(defaultIp)
    )

  private def getImageSnippetLogicInternal(
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight],
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] = {
    for {
      searchService <- ZIO.service[SearchService]
      imageSnippet <- searchService.getImageSnippet(docRef, startOffset, endOffset, highlights)
      stream <- ZIO.attempt {
        val out = new ByteArrayOutputStream()
        ImageIO.write(imageSnippet, "png", out)
        val in = new ByteArrayInputStream(out.toByteArray)
        ZStream.fromInputStream(in)
      }
    } yield stream
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to get image snippet", error))
    .mapError(mapToHttpError)

  def getImageSnippetWithHighlightsLogic(
      token: ValidToken,
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, ImageSnippetResponse] =
    getImageSnippetWithHighlightsLogicInternal(docRef, startOffset, endOffset, highlights, ipAddress, token.username)

  def getImageSnippetWithHighlightsLogicNoAuth(
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, ImageSnippetResponse] =
    getImageSnippetWithHighlightsLogicInternal(
      docRef,
      startOffset,
      endOffset,
      highlights,
      ipAddress,
      ipAddress.getOrElse(defaultIp)
    )

  private def getImageSnippetWithHighlightsLogicInternal(
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight],
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, ImageSnippetResponse] = {
    for {
      searchService <- ZIO.service[SearchService]
      imageSnippetAndHighlights <- searchService.getImageSnippetAndHighlights(
        docRef,
        startOffset,
        endOffset,
        highlights
      )
      response <- ZIO.attempt {
        imageSnippetAndHighlights match {
          case (imageSnippet, highlights) =>
            val out = new ByteArrayOutputStream()
            ImageIO.write(imageSnippet, "png", out)
            val bytes = out.toByteArray
            val encoder = Base64.getEncoder()
            val base64Image = encoder.encodeToString(bytes)
            ImageSnippetResponse(base64Image, highlights)
        }
      }
    } yield response
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to get image snippet with highlights", error))
    .mapError(mapToHttpError)

  def getAggregateLogic(
      token: ValidToken,
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Option[Boolean],
      strict: Option[Boolean],
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      field: String,
      maxBins: Option[Int],
      sortByLabel: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, AggregationBins] =
    getAggregateLogicInternal(
      query,
      title,
      authors,
      authorInclude,
      strict,
      fromYear,
      toYear,
      docRefs,
      field,
      maxBins,
      sortByLabel,
      ipAddress,
      token.username
    )

  def getAggregateLogicNoAuth(
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Option[Boolean],
      strict: Option[Boolean],
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      field: String,
      maxBins: Option[Int],
      sortByLabel: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, AggregationBins] =
    getAggregateLogicInternal(
      query,
      title,
      authors,
      authorInclude,
      strict,
      fromYear,
      toYear,
      docRefs,
      field,
      maxBins,
      sortByLabel,
      ipAddress,
      ipAddress.getOrElse(defaultIp)
    )

  private def getAggregateLogicInternal(
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Option[Boolean],
      strict: Option[Boolean],
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      field: String,
      maxBins: Option[Int],
      sortByLabel: Option[Boolean],
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, AggregationBins] = {
    for {
      indexField <- ZIO.attempt {
        try {
          IndexField.withName(field)
        } catch {
          case ex: NoSuchElementException => throw new UnknownIndexFieldException(field)
        }
      }
      searchQuery <- getSearchQuery(query, title, authors, authorInclude, strict, fromYear, toYear, docRefs)
      searchService <- ZIO.service[SearchService]
      searchResponse <- searchService.aggregate(searchQuery, indexField, maxBins, sortByLabel.getOrElse(false))
    } yield searchResponse
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to aggregate", error))
    .mapError(mapToHttpError)

  def getTopAuthorsLogic(
      token: ValidToken,
      prefix: String,
      maxBins: Option[Int],
      includeAuthor: Option[Boolean],
      includeAuthorInTranscription: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, AggregationBins] =
    getTopAuthorsLogicInternal(prefix, maxBins, includeAuthor, includeAuthorInTranscription, ipAddress, token.username)

  def getTopAuthorsLogicNoAuth(
      prefix: String,
      maxBins: Option[Int],
      includeAuthor: Option[Boolean],
      includeAuthorInTranscription: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, AggregationBins] =
    getTopAuthorsLogicInternal(
      prefix,
      maxBins,
      includeAuthor,
      includeAuthorInTranscription,
      ipAddress,
      ipAddress.getOrElse(defaultIp)
    )

  private def getTopAuthorsLogicInternal(
      prefix: String,
      maxBins: Option[Int],
      includeAuthor: Option[Boolean],
      includeAuthorInTranscription: Option[Boolean],
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, AggregationBins] = {
    for {
      searchService <- ZIO.service[SearchService]
      searchResponse <- searchService.getTopAuthors(
        prefix,
        maxBins,
        includeAuthor.getOrElse(true),
        includeAuthorInTranscription.getOrElse(true)
      )
    } yield searchResponse
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to get top authors", error))
    .mapError(mapToHttpError)

  def getTextAsHtmlLogic(
      token: ValidToken,
      docRef: DocReference,
      query: Option[String],
      strict: Option[Boolean],
      simplifyText: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    getTextAsHtmlLogicInternal(docRef, query, strict, simplifyText, ipAddress, token.username)

  def getTextAsHtmlLogicNoAuth(
      docRef: DocReference,
      query: Option[String],
      strict: Option[Boolean],
      simplifyText: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    getTextAsHtmlLogicInternal(docRef, query, strict, simplifyText, ipAddress, ipAddress.getOrElse(defaultIp))

  private def getTextAsHtmlLogicInternal(
      docRef: DocReference,
      query: Option[String],
      strict: Option[Boolean],
      simplifyText: Option[Boolean],
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    (for {
      searchService <- ZIO.service[SearchService]
      searchQuery <- getSearchQuery(
        query = query,
        strict = strict,
        matchAllDocuments = true
      )
      textAsHtml <- searchService.getTextAsHtml(docRef, Some(searchQuery), simplifyText.getOrElse(false))
    } yield {
      ZStream(textAsHtml)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to get text", error))
      .mapError(mapToHttpError)

  def getTextLogic(
      token: ValidToken,
      docRef: DocReference,
      dehyphenate: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    getTextLogicInternal(docRef, dehyphenate, ipAddress, token.username)

  def getTextLogicNoAuth(
      docRef: DocReference,
      dehyphenate: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    getTextLogicInternal(docRef, dehyphenate, ipAddress, ipAddress.getOrElse(defaultIp))

  private def getTextLogicInternal(
      docRef: DocReference,
      dehyphenate: Option[Boolean],
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    (for {
      searchService <- ZIO.service[SearchService]
      text <- searchService.getText(docRef, dehyphenate.getOrElse(false))
    } yield {
      ZStream(text)
        .via(ZPipeline.utf8Encode)
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to get text", error))
      .mapError(mapToHttpError)

  def getHighlightedTextLogic(
      token: ValidToken,
      docRef: DocReference,
      query: Option[String],
      strict: Option[Boolean],
      textAsHtml: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, HighlightedDocument] =
    getHighlightedTextLogicInternal(docRef, query, strict, textAsHtml, ipAddress, token.username)

  def getHighlightedTextLogicNoAuth(
      docRef: DocReference,
      query: Option[String],
      strict: Option[Boolean],
      textAsHtml: Option[Boolean],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, HighlightedDocument] =
    getHighlightedTextLogicInternal(docRef, query, strict, textAsHtml, ipAddress, ipAddress.getOrElse(defaultIp))

  private def getHighlightedTextLogicInternal(
      docRef: DocReference,
      query: Option[String],
      strict: Option[Boolean],
      textAsHtml: Option[Boolean],
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, HighlightedDocument] =
    (for {
      searchService <- ZIO.service[SearchService]
      searchQuery <- getSearchQuery(
        query = query,
        strict = strict,
        matchAllDocuments = true
      )
      highlightedDoc <- searchService.highlightDocument(
        docRef,
        Some(searchQuery),
        textAsHtml.getOrElse(false),
        simplifyText = false
      )
    } yield {
      highlightedDoc
    })
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to get highlighted document", error))
      .mapError(mapToHttpError)

  def getWordTextLogic(
      token: ValidToken,
      docRef: DocReference,
      wordOffset: Int,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, WordText] =
    getWordTextLogicInternal(docRef, wordOffset, ipAddress, token.username)

  def getWordTextLogicNoAuth(
      docRef: DocReference,
      wordOffset: Int,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, WordText] =
    getWordTextLogicInternal(docRef, wordOffset, ipAddress, ipAddress.getOrElse(defaultIp))

  private def getWordTextLogicInternal(
      docRef: DocReference,
      wordOffset: Int,
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, WordText] = {
    for {
      searchService <- ZIO.service[SearchService]
      text <- searchService.getWordText(docRef, wordOffset)
    } yield WordText(text)
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to get word text", error))
    .mapError(mapToHttpError)

  def getWordImageLogic(
      token: ValidToken,
      docRef: DocReference,
      wordOffset: Int,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    getWordImageLogicInternal(docRef, wordOffset, ipAddress, token.username)

  def getWordImageLogicNoAuth(
      docRef: DocReference,
      wordOffset: Int,
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    getWordImageLogicInternal(docRef, wordOffset, ipAddress, ipAddress.getOrElse(defaultIp))

  private def getWordImageLogicInternal(
      docRef: DocReference,
      wordOffset: Int,
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] = {
    for {
      searchService <- ZIO.service[SearchService]
      imageSnippet <- searchService.getWordImage(docRef, wordOffset)
      stream <- ZIO.attempt {
        val out = new ByteArrayOutputStream()
        ImageIO.write(imageSnippet, "png", out)
        val in = new ByteArrayInputStream(out.toByteArray)
        ZStream.fromInputStream(in)
      }
    } yield stream
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to get image snippet", error))
    .mapError(mapToHttpError)

  def getSizeLogic(): ZIO[Requirements, HttpError, SizeResponse] = {
    for {
      searchService <- ZIO.service[SearchService]
      size <- searchService.getIndexSize()
    } yield SizeResponse(size)
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to get index size", error))
    .mapError(mapToHttpError)

  def getListLogic(
      token: ValidToken,
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Option[Boolean],
      strict: Option[Boolean],
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      ocrSoftware: Option[String],
      sort: Option[String],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, Seq[DocReference]] =
    getListLogicInternal(
      query,
      title,
      authors,
      authorInclude,
      strict,
      fromYear,
      toYear,
      docRefs,
      ocrSoftware,
      sort,
      ipAddress,
      token.username
    )

  def getListLogicNoAuth(
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Option[Boolean],
      strict: Option[Boolean],
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      ocrSoftware: Option[String],
      sort: Option[String],
      ipAddress: Option[String]
  ): ZIO[Requirements, HttpError, Seq[DocReference]] =
    getListLogicInternal(
      query,
      title,
      authors,
      authorInclude,
      strict,
      fromYear,
      toYear,
      docRefs,
      ocrSoftware,
      sort,
      ipAddress,
      ipAddress.getOrElse(defaultIp)
    )

  private def getListLogicInternal(
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Option[Boolean],
      strict: Option[Boolean],
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      ocrSoftware: Option[String],
      sort: Option[String],
      ipAddress: Option[String],
      logUser: String
  ): ZIO[Requirements, HttpError, Seq[DocReference]] = {
    for {
      searchQuery <- getSearchQuery(
        query,
        title,
        authors,
        authorInclude,
        strict,
        fromYear,
        toYear,
        docRefs,
        ocrSoftware,
        matchAllDocuments = true
      )
      searchService <- ZIO.service[SearchService]
      parsedSort <- ZIO.attempt {
        try {
          sort.map(SortKind.withName).getOrElse(SortKind.DocReference).toSort
        } catch {
          case nsee: NoSuchElementException => throw new UnknownSortException(nsee.getMessage)
        }
      }
      docRefs <- searchService.list(
        searchQuery,
        parsedSort
      )
    } yield docRefs
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to list", error))
    .mapError(mapToHttpError)

  private def getSearchQuery(
      query: Option[String] = None,
      title: Option[String] = None,
      authors: List[String] = List.empty,
      authorInclude: Option[Boolean] = None,
      strict: Option[Boolean] = None,
      fromYear: Option[Int] = None,
      toYear: Option[Int] = None,
      docRefs: List[String] = List.empty,
      ocrSoftware: Option[String] = None,
      matchAllDocuments: Boolean = false
  ): Task[SearchQuery] = ZIO.attempt {
    val effectiveStrict = strict.getOrElse(false)
    val effectiveAuthorInclude = authorInclude.getOrElse(true)
    val criteria = Seq(
      query.flatMap(v =>
        Option.when(v.nonEmpty)(SearchCriterion.Contains(IndexField.Text, v, strict = effectiveStrict))
      ),
      title.flatMap(v =>
        Option.when(v.nonEmpty)(
          SearchCriterion.Contains(Seq(IndexField.Title, IndexField.TitleEnglish), v, strict = effectiveStrict)
        )
      ),
      Option.when(authors.nonEmpty) {
        val authorCriterion = SearchCriterion.ValueIn(IndexField.Author, authors)
        val authorEnglishCriterion = SearchCriterion.ValueIn(IndexField.AuthorEnglish, authors)
        val orCriterion = SearchCriterion.Or(authorCriterion, authorEnglishCriterion)
        if (effectiveAuthorInclude) {
          orCriterion
        } else {
          SearchCriterion.Not(orCriterion)
        }
      },
      Option.when(docRefs.nonEmpty) {
        SearchCriterion.ValueIn(IndexField.Reference, docRefs)
      },
      fromYear.map(SearchCriterion.GreaterThanOrEqualTo(IndexField.PublicationYearAsNumber, _)),
      toYear.map(SearchCriterion.LessThanOrEqualTo(IndexField.PublicationYearAsNumber, _)),
      ocrSoftware.map { ocrSoftware =>
        SearchCriterion.ValueIn(IndexField.OCRSoftware, Seq(ocrSoftware))
      }
    ).flatten

    val criterion = if (criteria.isEmpty) {
      if (matchAllDocuments) {
        SearchCriterion.MatchAllDocuments
      } else {
        throw new NoSearchCriteriaException
      }
    } else if (criteria.length == 1) {
      criteria.head
    } else {
      SearchCriterion.And(criteria*)
    }

    SearchQuery(criterion)
  }
}
