package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core._
import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.service.SearchServiceTest.{
  alternativeMap,
  alto1,
  alto2,
  docRef1,
  docRef2,
  ipAddress,
  metadata1,
  metadata2,
  textToAlto,
  username
}
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, ZIO, ZLayer}

import java.time.Instant
import scala.util.Using

object SearchServiceAnomalyTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndex with AltoHelper {
  private val languageSpecificFilterLayer = ZLayer.succeed(LanguageSpecificFilters.default)

  private val docRef1 = DocReference("doc1")
  private val metadata1 =
    DocMetadata(
      title = Some("Hello World"),
      author = Some("Joe Schmoe"),
      authorEnglish = Some("דשאָו שמאָו"),
      publicationYear = Some("1917")
    )
  private val alto1 = textToAlto(
    "doc1",
    "Cat Dog",
    Map.empty
  )

  private val docRef2 = DocReference("doc2")
  private val metadata2 =
    DocMetadata(
      title = Some("Cat"),
      author = Some("Joe Schmoe"),
      authorEnglish = Some("דשאָו שמאָו"),
      publicationYear = Some("1917")
    )
  private val alto2 = textToAlto(
    "doc2",
    "Cat Mouse",
    Map.empty
  )

  private val username = "jimi@hendrix.org"
  private val ipAddress = Some("127.0.0.1")

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("SearchServiceTest")(
    test("throw correct error on unparsable query") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        index <- ZIO.service[JochreIndex]
        exception <- ZIO.attempt {
          val query = SearchQuery(SearchCriterion.Contains(IndexField.Text, "\""))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }.flip
      } yield {
        assertTrue(exception match {
          case _: UnparsableQueryException => true
          case _                           => false
        })
      }
    },
    test("correct pretty quotes") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        pageCount1 <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        pageCount2 <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        resultsCatDog <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "„Cat Dog“")),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(0),
          "test",
          addOffsets = false
        )
        resultsCatDogNoQuotes <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "Cat Dog")),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(0),
          "test",
          addOffsets = false
        )
      } yield {
        assertTrue(pageCount1 == 1) &&
        assertTrue(pageCount2 == 1) &&
        assertTrue(resultsCatDog.results.map(_.docRef) == Seq(docRef1)) &&
        assertTrue(resultsCatDogNoQuotes.results.map(_.docRef) == Seq(docRef1, docRef2))
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ suggestionRepoLayer ++ indexLayer ++ languageSpecificFilterLayer) >>> SearchService.live ++ ZLayer
      .service[SearchRepo] ++ ZLayer
      .service[SuggestionRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
