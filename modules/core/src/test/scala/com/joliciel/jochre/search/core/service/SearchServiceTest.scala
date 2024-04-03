package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.{AggregationBin, AggregationBins, Contains, DocMetadata, DocReference, IndexField, SearchQuery}
import org.slf4j.LoggerFactory
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, ZIO, ZLayer}

import scala.util.Using

object SearchServiceTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndex with AltoHelper {
  private val log = LoggerFactory.getLogger(getClass)

  private val alternativeMap = Map(
    "hello" -> Seq("hi", "howdy"),
    "nice" -> Seq("pleasant", "lovely")
  )

  private val docRef1 = DocReference("doc1")
  private val metadata1 = DocMetadata(title = "Hello World", author = Some("Joe Schmoe"))
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
  private val metadata2 = DocMetadata(title = "Hello people", author = Some("Jack Sprat"))
  private val alto2 = textToAlto(
    "doc2",
    "Hello people.\n" +
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
      "the day after",
    alternativeMap
  )

  private val docRef3 = DocReference("doc3")
  private val metadata3 = DocMetadata(title = "Hi everyone", author = Some("Joe Schmoe"))
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
          val query = SearchQuery(Contains("world"))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
        refsHello <- ZIO.attempt {
          val query = SearchQuery(Contains("Hello"))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
        refsHiWorld <- ZIO.attempt {
          val query = SearchQuery(Contains("\"Hi World\""))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
      } yield {
        assertTrue(pageCount1 == 1) &&
        assertTrue(pageCount2 == 3) &&
        assertTrue(refsWorld == Seq(docRef1)) &&
        assertTrue(refsHiWorld == Seq(docRef1)) &&
        assertTrue(refsHello.toSet == Set(docRef1, docRef2))
      }
    },
    test("search single occurrence without padding") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultAre <- searchService.search(SearchQuery(Contains("are")), 0, 100, Some(1), Some(0), "test")
      } yield {
        assertTrue(resultAre.results.head.snippets.head.text == "How <b>are</b> you?")
      }
    },
    test("search single occurrence with padding") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultArePadding <- searchService.search(SearchQuery(Contains("are")), 0, 100, Some(1), Some(1), "test")
      } yield {
        assertTrue(
          resultArePadding.results.head.snippets.head.text == "Hello people.<br>" +
            "How <b>are</b> you?<br>" +
            "Fine, thank you."
        )
      }
    },
    test("search single occurrence with hyphen") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultAre <- searchService.search(SearchQuery(Contains("today")), 0, 100, Some(1), Some(1), "test")
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
          SearchQuery(Contains("\"will rain tomorrow\"")),
          0,
          100,
          Some(1),
          Some(1),
          "test"
        )
        phraseWithHyphenResult <- searchService.search(
          SearchQuery(Contains("\"day today Madam\"")),
          0,
          100,
          Some(1),
          Some(1),
          "test"
        )
      } yield {
        assertTrue(
          phraseResult.results.head.snippets.head.text == "Think it <b>will</b> <b>rain</b><br>" +
            "<b>tomorrow</b>? Oh no, I<br>" +
            "don't think so."
        ) &&
        assertTrue(
          phraseWithHyphenResult.results.head.snippets.head.text == "Hello you.<br>" +
            "Nice <b>day</b> <b>to-</b><br>" +
            "<b>day</b>, <b>Madam</b>.<br>" +
            "Isn't it?"
        )
      }
    },
    test("search multiple occurrence without padding") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultsThink <- searchService.search(SearchQuery(Contains("think")), 0, 100, Some(100), Some(0), "test")
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
        resultsThinkPadding <- searchService.search(SearchQuery(Contains("think")), 0, 100, Some(100), Some(1), "test")
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
    test("aggregate") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        _ <- searchService.indexAlto(docRef3, alto3, metadata3)
        binsHello <- searchService.aggregate(SearchQuery(Contains("Hello")), IndexField.Author, 2)
        binsHello1 <- searchService.aggregate(SearchQuery(Contains("Hello")), IndexField.Author, 1)
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
      } yield {
        assertTrue(binsJ == AggregationBins(Seq(AggregationBin("Jack Sprat", 1), AggregationBin("Joe Schmoe", 2)))) &&
        assertTrue(binsJo == AggregationBins(Seq(AggregationBin("Joe Schmoe", 2)))) &&
        assertTrue(binsJ1 == AggregationBins(Seq(AggregationBin("Joe Schmoe", 2))))
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ indexLayer) >>> SearchService.live ++ ZLayer.service[SearchRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
