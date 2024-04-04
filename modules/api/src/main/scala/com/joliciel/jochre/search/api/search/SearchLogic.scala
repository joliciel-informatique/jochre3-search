package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper}
import com.joliciel.jochre.search.core.service.{Highlight, SearchResponse, SearchService}
import com.joliciel.jochre.search.core.{
  AggregationBins,
  DocReference,
  IndexField,
  NoSearchCriteriaException,
  SearchCriterion,
  SearchQuery,
  UnknownFieldException
}
import zio.{Task, ZIO}
import zio.stream.ZStream

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.imageio.ImageIO

trait SearchLogic extends HttpErrorMapper {
  def getSearchLogic(
      token: ValidToken,
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Boolean,
      strict: Boolean,
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      first: Int,
      max: Int,
      maxSnippets: Option[Int],
      rowPadding: Option[Int],
      sort: Option[String]
  ): ZIO[Requirements, HttpError, SearchResponse] = {
    for {
      searchQuery <- getSearchQuery(query, title, authors, authorInclude, strict, fromYear, toYear, docRefs)
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
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Boolean,
      strict: Boolean,
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String],
      field: String,
      maxBins: Int
  ): ZIO[Requirements, HttpError, AggregationBins] = {
    for {
      indexField <- ZIO.attempt {
        try {
          IndexField.withName(field)
        } catch {
          case ex: NoSuchElementException => throw new UnknownFieldException(ex.getMessage)
        }
      }
      searchQuery <- getSearchQuery(query, title, authors, authorInclude, strict, fromYear, toYear, docRefs)
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

  private def getSearchQuery(
      query: Option[String],
      title: Option[String],
      authors: List[String],
      authorInclude: Boolean,
      strict: Boolean,
      fromYear: Option[Int],
      toYear: Option[Int],
      docRefs: List[String]
  ): Task[SearchQuery] = ZIO.attempt {
    val criteria = Seq(
      query.map(SearchCriterion.Contains(IndexField.Text, _, strict = strict)),
      title.map(SearchCriterion.Contains(Seq(IndexField.Title, IndexField.TitleEnglish), _, strict = strict)),
      Option.when(authors.nonEmpty) {
        val authorCriterion = SearchCriterion.ValueIn(IndexField.Author, authors)
        val authorEnglishCriterion = SearchCriterion.ValueIn(IndexField.AuthorEnglish, authors)
        val orCriterion = SearchCriterion.Or(authorCriterion, authorEnglishCriterion)
        if (authorInclude) {
          orCriterion
        } else {
          SearchCriterion.Not(orCriterion)
        }
      },
      Option.when(docRefs.nonEmpty) {
        SearchCriterion.ValueIn(IndexField.Reference, docRefs)
      },
      fromYear.map(SearchCriterion.GreaterThanOrEqualTo(IndexField.PublicationYearAsNumber, _)),
      toYear.map(SearchCriterion.LessThanOrEqualTo(IndexField.PublicationYearAsNumber, _))
    ).flatten

    val criterion = if (criteria.length == 0) {
      throw new NoSearchCriteriaException(f"No search criteria")
    } else if (criteria.length == 1) {
      criteria(0)
    } else {
      SearchCriterion.And(criteria: _*)
    }

    SearchQuery(criterion)
  }
}
