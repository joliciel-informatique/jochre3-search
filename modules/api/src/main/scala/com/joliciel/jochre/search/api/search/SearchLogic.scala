package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import zio.ZIO
import zio.stream.ZStream

trait SearchLogic extends HttpErrorMapper {
  def getSearchLogic(
      token: ValidToken,
      query: String,
      first: Int,
      max: Int,
      maxSnippets: Option[Int],
      rowPadding: Option[Int]
  ): ZIO[Requirements, HttpError, SearchResponse] =
    ZIO.succeed(SearchHelper.searchResponseExample)

  def getImageSnippetLogic(
      token: ValidToken,
      docId: DocId,
      page: Int,
      startLine: Int,
      endLine: Int,
      highlights: Seq[Highlight]
  ): ZIO[Requirements, HttpError, ZStream[Any, Throwable, Byte]] =
    ZIO
      .attempt {
        val inputStream = getClass.getClassLoader.getResourceAsStream("image_snippet_example.png")
        ZStream.fromInputStream(inputStream)
      }
      .tapErrorCause(error => ZIO.logErrorCause(s"Unable to get batch", error))
      .mapError(mapToHttpError)
}
