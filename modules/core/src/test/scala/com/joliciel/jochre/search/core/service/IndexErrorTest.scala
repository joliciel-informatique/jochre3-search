package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core._
import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, ZIO, ZLayer}

import java.time.Instant
import scala.util.Using

object IndexErrorTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndex with AltoHelper {
  private val languageSpecificFilterLayer = ZLayer.succeed(LanguageSpecificFilters.default)
  private val alternativeMap = Map(
    "hello" -> Seq("hi", "howdy"),
    "nice" -> Seq("pleasant", "lovely")
  )

  private val docRef1 = DocReference("doc1")
  private val metadata1 =
    DocMetadata(
      titleEnglish = Some("\"Test quote\""),
      title = Some("""\"Test quote\""""),
      author = Some("Joe Schmoe"),
      authorEnglish = Some("דשאָו שמאָו"),
      publicationYear = Some("1917")
    )
  private val alto1 = textToAlto(
    "doc1",
    "Dog",
    alternativeMap
  )

  private val docRef2 = DocReference("doc2")
  private val metadata2 =
    DocMetadata(
      title = Some("\"דער קעניג פון יהודה : פון דער סעריע \"נביאים"),
      titleEnglish = Some("Der Ḳenig fun Yehudah fun der serye \"neviim\""),
      author = Some("Jack Sprat"),
      publicationYear = Some("[192_]")
    )

  private val text2 = "Hello people.\n" +
    "Cat"

  private val alto2 = textToAlto(
    "doc2",
    text2,
    alternativeMap
  )

  private val username = "jimi@hendrix.org"
  private val ipAddress = Some("127.0.0.1")

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("IndexErrorTest")(
    test("index alto file") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        pageCount1 <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        pageCount2 <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        index <- ZIO.service[JochreIndex]
        refsDog <- ZIO.attempt {
          val query = SearchQuery(SearchCriterion.Contains(IndexField.Text, "dog"))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }

      } yield {
        assertTrue(pageCount1 == 1) &&
        assertTrue(pageCount2 == 1) &&
        assertTrue(refsDog == Seq(docRef1))
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ suggestionRepoLayer ++ indexLayer ++ languageSpecificFilterLayer) >>> SearchService.live ++ ZLayer
      .service[SearchRepo] ++ ZLayer
      .service[SuggestionRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
