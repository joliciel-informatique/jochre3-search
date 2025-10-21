package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core._
import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, ZIO, ZLayer}

import java.time.Instant
import scala.util.Using

object SuggestionTest
    extends JUnitRunnableSpec
    with DatabaseTestBase
    with WithTestIndexLayer
    with WithDehyphenatorLayer
    with AltoHelper {
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
    "tomorrow? Oh “Loki”, I\n" +
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
    test("make a suggestion") {
      val wordOffset = (docRef2.ref + "\n\n" +
        "Hello people.\n" +
        "How are you?\n\n" +
        "Fine, thank you.\n\n" +
        "With pleasure.\n" +
        "\n\n" +
        "Think it will rain\n" +
        "tomorrow? Oh “").length
      for {
        _ <- getSuggestionRepo()
        searchRepo <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        _ <- searchService.addFakeDocument(docRef3, username, ipAddress, alto3, metadata3)
        originalTextBefore <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "Loki")),
          Sort.Score,
          0,
          100,
          Some(100),
          Some(0),
          "joe",
          addOffsets = false
        )
        _ <- searchService.suggestWord(username, ipAddress, docRef2, wordOffset, "“Beelzebub”,")
        _ <- searchService.reindexWhereRequired()
        replacementTextAfter <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "Beelzebub")),
          Sort.Score,
          0,
          100,
          Some(100),
          Some(0),
          "joe",
          addOffsets = false
        )
        originalTextAfter <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "Loki")),
          Sort.Score,
          0,
          100,
          Some(100),
          Some(0),
          "joe",
          addOffsets = false
        )
        // Ensure we can find the word in the database (required for image snippet generation)
        word <- searchRepo.getWord(
          replacementTextAfter.results.head.docRev,
          replacementTextAfter.results.head.snippets.head.highlights.head.start
        )
        ignoreCount <- searchService.ignoreSuggestions(username)
        _ <- searchService.reindexWhereRequired()
        originalTextAfterIgnore <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "Loki")),
          Sort.Score,
          0,
          100,
          Some(100),
          Some(0),
          "joe",
          addOffsets = false
        )
      } yield {
        assertTrue(replacementTextAfter.results.size == 1) &&
        assertTrue(originalTextAfter.results.size == originalTextBefore.results.size - 1) &&
        assertTrue(
          originalTextBefore.results.head.snippets.head.text == "<div class=\"text-snippet\">tomorrow? Oh “<b>Loki</b>”, I</div>"
        ) &&
        assertTrue(
          replacementTextAfter.results.head.snippets.head.text == "<div class=\"text-snippet\">tomorrow? Oh “<b>Beelzebub</b>”, I</div>"
        ) &&
        assertTrue(
          word.isDefined
        ) &&
        assertTrue(ignoreCount == 1) &&
        assertTrue(originalTextAfterIgnore.totalCount == originalTextBefore.totalCount)
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ suggestionRepoLayer ++ indexLayer ++ languageSpecificFilterLayer ++ dehyphenatorLayer) >>> SearchService.live ++ ZLayer
      .service[SearchRepo] ++ ZLayer
      .service[SuggestionRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
