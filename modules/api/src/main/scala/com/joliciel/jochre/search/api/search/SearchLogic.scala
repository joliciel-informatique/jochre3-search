package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper}
import com.joliciel.jochre.search.core.{
  AggregationBins,
  AuthorStartsWith,
  Contains,
  DocReference,
  IndexField,
  SearchQuery,
  UnknownFieldException
}
import com.joliciel.jochre.search.core.service.{Highlight, SearchResponse, SearchService}
import zio.ZIO
import zio.stream.ZStream

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.imageio.ImageIO

trait SearchLogic extends HttpErrorMapper {
  def getSearchLogic(
      token: ValidToken,
      query: String,
      first: Int,
      max: Int,
      maxSnippets: Option[Int],
      rowPadding: Option[Int]
  ): ZIO[Requirements, HttpError, SearchResponse] = {
    val searchQuery = SearchQuery(Contains(query))
    for {
      searchService <- ZIO.service[SearchService]
      searchResponse <- searchService.search(searchQuery, first, max, maxSnippets, rowPadding, token.username)
    } yield searchResponse
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to search", error))
    .mapError(mapToHttpError)

  def getImageSnippetLogic(
      token: ValidToken,
      docRef: DocReference,
      startOffset: Int,
      endOffset: Int,
      highlights: Seq[Highlight]
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

  def getAggregateLogic(
      token: ValidToken,
      query: String,
      field: String,
      maxBins: Int
  ): ZIO[Requirements, HttpError, AggregationBins] = {
    val searchQuery = SearchQuery(Contains(query))
    for {
      indexField <- ZIO.attempt {
        try {
          IndexField.withName(field)
        } catch {
          case ex: NoSuchElementException => throw new UnknownFieldException(ex.getMessage)
        }
      }
      searchService <- ZIO.service[SearchService]
      searchResponse <- searchService.aggregate(searchQuery, indexField, maxBins)
    } yield searchResponse
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to aggregate", error))
    .mapError(mapToHttpError)

  def getTopAuthorsLogic(
      token: ValidToken,
      prefix: String,
      maxBins: Int
  ): ZIO[Requirements, HttpError, AggregationBins] = {
    for {
      searchService <- ZIO.service[SearchService]
      searchResponse <- searchService.getTopAuthors(prefix, maxBins)
    } yield searchResponse
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to get top authors", error))
    .mapError(mapToHttpError)
}
