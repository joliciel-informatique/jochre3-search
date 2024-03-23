package com.joliciel.jochre.search.core.search

import com.joliciel.jochre.ocr.core.model.{Alto, Page, TextBlock}
import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.{DocMetadata, DocReference}
import zio.{Scope, ZIO, ZLayer}
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue, ignored}

import scala.util.Using

object SearchServiceTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndex with AltoHelper {
  val alternativeMap = Map(
    "hello" -> Seq("hi", "howdy"),
    "nice" -> Seq("pleasant", "lovely")
  )

  val docRef1 = DocReference("doc1")
  val metadata1 = DocMetadata(title = "Hello World")

  val alto1 = textToAlto(
    "doc1",
    "Hello world!\n" +
      "Nice day to-\n" +
      "day",
    alternativeMap
  )

  val docRef2 = DocReference("doc2")
  val metadata2 = DocMetadata(title = "Hello people")

  val alto2 = textToAlto(
    "doc2",
    "Hello people.\n" +
      "How are you?\n" +
      "Fine, thank you.\n" +
      "With pleasure.\n" +
      "\n" +
      "Think it will rain\n" +
      "today? Oh no, I\n" +
      "don't think so.\n" +
      "\n" +
      "I think it will be\n" +
      "sunny today, and even\n" +
      "tomorrow",
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
        resultAre <- searchService.search("are", 0, 100, Some(1), Some(0), "test")
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
        resultArePadding <- searchService.search("are", 0, 100, Some(1), Some(1), "test")
      } yield {
        assertTrue(
          resultArePadding.results.head.snippets.head.text == "Hello people.\nHow <b>are</b> you?\nFine, thank you."
        )
      }
    },
    test("search multiple occurrence without padding") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultsThink <- searchService.search("think", 0, 100, Some(100), Some(0), "test")
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
        resultsThinkPadding <- searchService.search("think", 0, 100, Some(100), Some(1), "test")
      } yield {
        assertTrue(
          resultsThinkPadding.results.head.snippets.sortBy(_.start).map(_.text) == Seq(
            "<b>Think</b> it will rain\ntoday? Oh no, I\ndon't <b>think</b> so.",
            "I <b>think</b> it will be\nsunny today, and even"
          )
        )
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ indexLayer) >>> SearchService.live ++ ZLayer.service[SearchRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
