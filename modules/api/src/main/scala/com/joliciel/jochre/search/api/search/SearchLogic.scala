package com.joliciel.jochre.search.api.search

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper}
import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.search.{Highlight, SearchResponse, SearchService}
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
    for {
      searchService <- ZIO.service[SearchService]
      searchResponse <- searchService.search(query, first, max, maxSnippets, rowPadding, token.username)
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
}
