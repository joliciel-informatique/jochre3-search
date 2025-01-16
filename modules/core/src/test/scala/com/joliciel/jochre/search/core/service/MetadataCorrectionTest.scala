package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core._
import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, ZIO, ZLayer}

import java.time.Instant
import scala.util.Using

object MetadataCorrectionTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndexLayer with AltoHelper {
  private val languageSpecificFilterLayer = ZLayer.succeed(LanguageSpecificFilters.default)
  private val alternativeMap = Map(
    "hello" -> Seq("hi", "howdy"),
    "nice" -> Seq("pleasant", "lovely")
  )

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
    "Hello world!\n\n" +
      "Hello you.\n\n" +
      "Nice day to-\n" +
      "day, Madam.\n" +
      "Isn't it?\n\n" +
      "Oh yes, it is.",
    alternativeMap
  )

  private val docRef2 = DocReference("doc2")
  private val metadata2 =
    DocMetadata(title = Some("Hello people"), author = Some("Jack Sprat"), publicationYear = Some("[192_]"))

  private val text2 = "Hello people.\n" +
    "How are you?\n\n" +
    "Fine, thank you.\n\n" +
    "With pleasure.\n" +
    "\n\n" +
    "Think it will rain\n" +
    "tomorrow? Oh no, I\n" +
    "don't think so.\n" +
    "\n\n" +
    "I think it will be\n" +
    "sunny tomorrow, and even\n" +
    "the day after"

  private val alto2 = textToAlto(
    "doc2",
    text2,
    alternativeMap
  )

  private val docRef3 = DocReference("doc3")
  private val metadata3 =
    DocMetadata(title = Some("Hi everyone"), author = Some("Joe Schmoe"), publicationYear = Some("1937"))
  private val alto3 = textToAlto(
    "doc3",
    "Hello everyone",
    alternativeMap
  )


  private val username = "jimi@hendrix.org"
  private val ipAddress = Some("127.0.0.1")

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SearchServiceTest")(
    test("correct metadata") {
      for {
        _ <- getSuggestionRepo()
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        _ <- searchService.addFakeDocument(docRef3, username, ipAddress, alto3, metadata3)
        correctionId <- searchService.correctMetadata(
          username,
          ipAddress,
          docRef1,
          MetadataField.Author,
          "Joseph Schmozeph",
          applyEverywhere = true
        )
        _ <- searchService.reindexWhereRequired()
        resultsSchmozeph <- searchService.search(
          SearchQuery(SearchCriterion.ValueIn(IndexField.Author, Seq("Joseph Schmozeph")))
        )
        _ <- searchService.undoMetadataCorrection(correctionId)
        _ <- searchService.reindexWhereRequired()
        resultsSchmozephAfterUndo <- searchService.search(
          SearchQuery(SearchCriterion.ValueIn(IndexField.Author, Seq("Joseph Schmozeph")))
        )
        resultsSchmoeAfterUndo <- searchService.search(
          SearchQuery(SearchCriterion.ValueIn(IndexField.Author, Seq("Joe Schmoe")))
        )
      } yield {
        assertTrue(resultsSchmozeph.results.map(_.docRef).toSet == Set(docRef1, docRef3)) &&
        assertTrue(resultsSchmozephAfterUndo.results.map(_.docRef) == Seq()) &&
        assertTrue(resultsSchmoeAfterUndo.results.map(_.docRef).toSet == Set(docRef1, docRef3))
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ suggestionRepoLayer ++ indexLayer ++ languageSpecificFilterLayer) >>> SearchService.live ++ ZLayer
      .service[SearchRepo] ++ ZLayer
      .service[SuggestionRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
