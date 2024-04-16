package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.{
  AggregationBin,
  AggregationBins,
  DocMetadata,
  DocReference,
  IndexField,
  SearchCriterion,
  SearchQuery,
  Sort
}
import org.slf4j.LoggerFactory
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, ZIO, ZLayer}

import java.time.Instant
import scala.util.Using

object SearchServiceTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndex with AltoHelper {
  private val log = LoggerFactory.getLogger(getClass)

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
    "Hello world!\n" +
      "Hello you.\n" +
      "Nice day to-\n" +
      "day, Madam.\n" +
      "Isn't it?\n" +
      "Oh yes, it is.",
    alternativeMap
  )

  private val docRef2 = DocReference("doc2")
  private val metadata2 =
    DocMetadata(title = Some("Hello people"), author = Some("Jack Sprat"), publicationYear = Some("[192_]"))

  private val text2 = "Hello people.\n" +
    "How are you?\n" +
    "Fine, thank you.\n" +
    "With pleasure.\n" +
    "\n" +
    "Think it will rain\n" +
    "tomorrow? Oh no, I\n" +
    "don't think so.\n" +
    "\n" +
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

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("SearchServiceTest")(
    test("index alto file") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        pageCount1 <- searchService.indexAlto(docRef1, alto1, metadata1)
        pageCount2 <- searchService.indexAlto(docRef2, alto2, metadata2)
        index <- ZIO.service[JochreIndex]
        refsWorld <- ZIO.attempt {
          val query = SearchQuery(SearchCriterion.Contains(IndexField.Text, "world", strict = false))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
        refsHello <- ZIO.attempt {
          val query = SearchQuery(SearchCriterion.Contains(IndexField.Text, "Hello", strict = false))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
        refsHiWorld <- ZIO.attempt {
          val query = SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"Hi World\"", strict = false))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
        textWithHtml <- searchService.getTextAsHtml(docRef2)
      } yield {
        assertTrue(pageCount1 == 1) &&
        assertTrue(pageCount2 == 3) &&
        assertTrue(refsWorld == Seq(docRef1)) &&
        assertTrue(refsHiWorld == Seq(docRef1)) &&
        assertTrue(refsHello.toSet == Set(docRef1, docRef2)) &&
        assertTrue(
          textWithHtml.replaceAll("<(.+?)>", "") == f"${metadata2.title.get}${text2.replaceAll("\n", "")}"
        )
      }
    },
    test("search single occurrence without padding") {
      val startTime = Instant.now()
      for {
        searchRepo <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultAre <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "are", strict = false)),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(0),
          "test",
          addOffsets = false
        )
        queries <- searchRepo.getQueriesSince(startTime)
      } yield {
        assertTrue(resultAre.results.head.snippets.head.text == "How <b>are</b> you?") &&
        assertTrue(queries.head.query == Some("are"))
      }
    },
    test("search single occurrence with padding") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultArePadding <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "are", strict = false)),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(1),
          "test",
          addOffsets = false
        )
        resultWithOffsets <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "are", strict = false)),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(1),
          "test",
          addOffsets = true
        )
      } yield {
        assertTrue(
          resultArePadding.results.head.snippets.head.text == "Hello people.<br>" +
            "How <b>are</b> you?<br>" +
            "Fine, thank you."
        ) &&
        assertTrue(
          resultWithOffsets.results.head.snippets.head.text ==
            """<span offset="5">Hello people.</span><br>
              |<span offset="19">How </span><b><span offset="23">are</span></b><span offset="26"> you?</span><br>
              |<span offset="32">Fine, thank you.</span>""".stripMargin.replaceAll("\n", "")
        )
      }
    },
    test("search single occurrence with hyphen") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultAre <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "today", strict = false)),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(1),
          "test",
          addOffsets = false
        )
      } yield {
        assertTrue(
          resultAre.results.head.snippets.head.text == "Hello you.<br>" +
            "Nice day <b>to-</b><br>" +
            "<b>day</b>, Madam.<br>" +
            "Isn't it?"
        )
      }
    },
    test("search phrase containing word with or without hyphen") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        phraseResult <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"will rain tomorrow\"", strict = false)),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(1),
          "test",
          addOffsets = false
        )
        phraseWithHyphenResult <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"day today Madam\"", strict = false)),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(1),
          "test",
          addOffsets = false
        )
      } yield {
        assertTrue(
          phraseResult.results.head.snippets.head.text == "Think it <b>will</b> <b>rain</b><br>" +
            "<b>tomorrow</b>? Oh no, I<br>" +
            "don't think so."
        ) &&
        assertTrue(
          phraseResult.results.head.snippets.head.page == 1
        ) &&
        assertTrue(
          phraseWithHyphenResult.results.head.snippets.head.text == "Hello you.<br>" +
            "Nice <b>day</b> <b>to-</b><br>" +
            "<b>day</b>, <b>Madam</b>.<br>" +
            "Isn't it?"
        ) &&
        assertTrue(
          phraseWithHyphenResult.results.head.snippets.head.page == 0
        )
      }
    },
    test("search multiple occurrence without padding") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultsThink <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "think", strict = false)),
          Sort.Score,
          0,
          100,
          Some(100),
          Some(0),
          "test",
          addOffsets = false
        )
      } yield {
        assertTrue(
          resultsThink.results.head.snippets.sortBy(_.start).map(_.text) == Seq(
            "<b>Think</b> it will rain",
            "don't <b>think</b> so.",
            "I <b>think</b> it will be"
          )
        )
      }
    },
    test("search multiple occurrence with padding") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultsThinkPadding <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "think", strict = false)),
          Sort.Score,
          0,
          100,
          Some(100),
          Some(1),
          "test",
          addOffsets = false
        )
      } yield {
        assertTrue(
          resultsThinkPadding.results.head.snippets.sortBy(_.start).map(_.text) == Seq(
            "<b>Think</b> it will rain<br>" +
              "tomorrow? Oh no, I<br>" +
              "don't <b>think</b> so.",
            "I <b>think</b> it will be<br>" +
              "sunny tomorrow, and even"
          )
        )
      }
    },
    test("various search criteria") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        _ <- searchService.indexAlto(docRef3, alto3, metadata3)
        titleContainsWorld <- searchService.search(
          SearchQuery(SearchCriterion.Contains(Seq(IndexField.Title, IndexField.TitleEnglish), "world", strict = false))
        )
        authorInJoe <- searchService.search(SearchQuery(SearchCriterion.ValueIn(IndexField.Author, Seq("Joe Schmoe"))))
        authorStartsWithJo <- searchService.search(SearchQuery(SearchCriterion.StartsWith(IndexField.Author, "Jo")))
        yearBefore1920 <- searchService.search(
          SearchQuery(SearchCriterion.LessThanOrEqualTo(IndexField.PublicationYearAsNumber, 1920))
        )
        yearAfter1920 <- searchService.search(
          SearchQuery(SearchCriterion.GreaterThanOrEqualTo(IndexField.PublicationYearAsNumber, 1920))
        )
        notAuthorInJoe <- searchService.search(
          SearchQuery(
            SearchCriterion.And(
              SearchCriterion.Contains(IndexField.Title, "Hello"),
              SearchCriterion.Not(SearchCriterion.ValueIn(IndexField.Author, Seq("Joe Schmoe")))
            )
          )
        )
        andYear <- searchService.search(
          SearchQuery(
            SearchCriterion.And(
              SearchCriterion.LessThanOrEqualTo(IndexField.PublicationYearAsNumber, 1920),
              SearchCriterion.GreaterThanOrEqualTo(IndexField.PublicationYearAsNumber, 1920)
            )
          )
        )
      } yield {
        assertTrue(titleContainsWorld.results.map(_.docRef).sortBy(_.ref) == Seq(docRef1)) &&
        assertTrue(authorInJoe.results.map(_.docRef).sortBy(_.ref) == Seq(docRef1, docRef3)) &&
        assertTrue(authorStartsWithJo.results.map(_.docRef).sortBy(_.ref) == Seq(docRef1, docRef3)) &&
        assertTrue(yearBefore1920.results.map(_.docRef).sortBy(_.ref) == Seq(docRef1, docRef2)) &&
        assertTrue(yearAfter1920.results.map(_.docRef).sortBy(_.ref) == Seq(docRef2, docRef3)) &&
        assertTrue(notAuthorInJoe.results.map(_.docRef).sortBy(_.ref) == Seq(docRef2)) &&
        assertTrue(andYear.results.map(_.docRef).sortBy(_.ref) == Seq(docRef2))
      }
    },
    test("aggregate") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        _ <- searchService.indexAlto(docRef3, alto3, metadata3)
        binsHello <- searchService.aggregate(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "Hello", strict = false)),
          IndexField.Author,
          2
        )
        binsHello1 <- searchService.aggregate(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "Hello", strict = false)),
          IndexField.Author,
          1
        )
      } yield {
        assertTrue(
          binsHello == AggregationBins(Seq(AggregationBin("Joe Schmoe", 2), AggregationBin("Jack Sprat", 1)))
        ) &&
        assertTrue(binsHello1 == AggregationBins(Seq(AggregationBin("Joe Schmoe", 2))))
      }
    },
    test("return top authors in alphabetical order") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        _ <- searchService.indexAlto(docRef3, alto3, metadata3)
        binsJ <- searchService.getTopAuthors("J", 5)
        binsJo <- searchService.getTopAuthors("Jo", 5)
        binsJ1 <- searchService.getTopAuthors("J", 1)
        binsDalet <- searchService.getTopAuthors("ד", 5)
      } yield {
        assertTrue(binsJ == AggregationBins(Seq(AggregationBin("Jack Sprat", 1), AggregationBin("Joe Schmoe", 2)))) &&
        assertTrue(binsJo == AggregationBins(Seq(AggregationBin("Joe Schmoe", 2)))) &&
        assertTrue(binsJ1 == AggregationBins(Seq(AggregationBin("Joe Schmoe", 2)))) &&
        assertTrue(binsDalet == AggregationBins(Seq(AggregationBin("דשאָו שמאָו", 1))))
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ indexLayer) >>> SearchService.live ++ ZLayer.service[SearchRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
