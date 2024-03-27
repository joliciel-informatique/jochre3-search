package com.joliciel.jochre.search.core.search

import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.{DocMetadata, DocReference}
import org.slf4j.LoggerFactory
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, ZIO, ZLayer}

import java.io.File
import javax.imageio.ImageIO
import scala.util.Using

object SearchServiceTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndex with AltoHelper {
  private val log = LoggerFactory.getLogger(getClass)

  val alternativeMap = Map(
    "hello" -> Seq("hi", "howdy"),
    "nice" -> Seq("pleasant", "lovely")
  )

  val docRef1 = DocReference("doc1")
  val metadata1 = DocMetadata(title = "Hello World")

  val alto1 = textToAlto(
    "doc1",
    "Hello world!\n" +
      "Hello you.\n" +
      "Nice day to-\n" +
      "day.\n" +
      "Isn't it?\n" +
      "Oh yes, it is.",
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
      "tomorrow? Oh no, I\n" +
      "don't think so.\n" +
      "\n" +
      "I think it will be\n" +
      "sunny tomorrow, and even\n" +
      "the day after",
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
    test("search single occurrence with hyphen") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultAre <- searchService.search("today", 0, 100, Some(1), Some(1), "test")
      } yield {
        assertTrue(
          resultAre.results.head.snippets.head.text == "Hello you.\nNice day <b>to-</b>\n<b>day</b>.\nIsn't it?"
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
            "<b>Think</b> it will rain\ntomorrow? Oh no, I\ndon't <b>think</b> so.",
            "I <b>think</b> it will be\nsunny tomorrow, and even"
          )
        )
      }
    },
    test("upload real pdf and get image snippets") {
      val docRef = DocReference("nybc200089")
      val pdfStream = getClass.getResourceAsStream("/nybc200089-11-12.pdf")

      val altoStream = getClass.getResourceAsStream("/nybc200089-11-12_alto4.zip")

      val metadataStream = getClass.getResourceAsStream("/nybc200089_meta.xml")

      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        pageCount <- searchService.indexPdf(docRef, pdfStream, altoStream, Some(metadataStream))
        searchResults <- searchService.search("velt", 0, 10, Some(20), Some(1), "test")
        topResult <- ZIO.attempt(searchResults.results.head)
        imageSnippet <- searchService.getImageSnippet(
          topResult.docRef,
          topResult.snippets.head.start,
          topResult.snippets.head.end,
          topResult.snippets.head.highlights
        )
      } yield {
        val tempFile = File.createTempFile("jochre-snippet", ".png")
        ImageIO.write(imageSnippet, "png", tempFile)
        log.info(f"Wrote snippet to ${tempFile.getPath}")
        assertTrue(searchResults.totalCount == 1) &&
        assertTrue(pageCount == 2)
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ indexLayer) >>> SearchService.live ++ ZLayer.service[SearchRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
