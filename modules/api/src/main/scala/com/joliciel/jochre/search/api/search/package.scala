package com.joliciel.jochre.search.api

package object search {
  case class DocId(id: String)

  case class SearchResponse(results: Seq[SearchResult], totalCount: Int)

  case class SearchResult(
      docId: DocId,
      score: Double,
      snippets: Seq[Snippet]
  )

  case class Snippet(
      text: String,
      start: Int,
      end: Int,
      page: Int,
      startLine: Int,
      endLine: Int,
      highlights: Seq[Highlight]
  )

  case class Highlight(start: Int, end: Int)

  object SearchHelper {
    val searchResponseExample: SearchResponse = SearchResponse(
      results = Seq(
        SearchResult(
          docId = DocId("nybc200089"),
          score = 0.90,
          snippets = Seq(
            Snippet(
              text = "אין דער <b>אַלטער הײם</b>.",
              start = 100,
              end = 118,
              page = 72,
              startLine = 11,
              endLine = 13,
              highlights = Seq(Highlight(108, 117))
            )
          )
        )
      ),
      totalCount = 42
    )
  }
}
