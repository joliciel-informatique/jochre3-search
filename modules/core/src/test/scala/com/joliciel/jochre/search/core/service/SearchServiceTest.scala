package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core._
import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, ZIO, ZLayer}

import java.time.Instant
import scala.util.Using

object SearchServiceTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndexLayer with AltoHelper {
  private val languageSpecificFilterLayer = ZLayer.succeed(LanguageSpecificFilters.default)
  private val alternativeMap = Map(
    "hello" -> Seq("hi", "howdy"),
    "nice" -> Seq("pleasant", "lovely")
  )

  private val docRef1 = DocReference("doc1")
  private val metadata1 =
    DocMetadata(
      title = Some("שלום־עליכם װעלט"),
      titleEnglish = Some("Hello World"),
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

  private val docRef4 = DocReference("doc4")
  private val metadata4 =
    DocMetadata(title = Some("With great pleasure"), author = Some("Joe Schmoe"), publicationYear = Some("1937"))
  private val alto4 = textToAlto(
    "doc4",
    "With pleasure.\n" +
      "With great pleasure.\n" +
      "Really.",
    alternativeMap
  )

  private val username = "jimi@hendrix.org"
  private val ipAddress = Some("127.0.0.1")

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SearchServiceTest")(
    test("index alto file") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        pageCount1 <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        pageCount2 <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        index <- ZIO.service[JochreIndex]
        refsWorld <- ZIO.attempt {
          val query = SearchQuery(SearchCriterion.Contains(IndexField.Text, "world"))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
        refsHello <- ZIO.attempt {
          val query = SearchQuery(SearchCriterion.Contains(IndexField.Text, "Hello"))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
        refsHiWorld <- ZIO.attempt {
          val query = SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"Hi World\""))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
        textWithHtml <- searchService.getTextAsHtml(docRef2, query = None, simplifyText = false)
        textWithHtmlHighlights <- searchService.getTextAsHtml(
          docRef2,
          query = Some(SearchQuery(SearchCriterion.Contains(IndexField.Text, "think"))),
          simplifyText = false
        )
        _ <- searchService.removeDocument(docRef1)
        refsHelloAfterRemove <- ZIO.attempt {
          val query = SearchQuery(SearchCriterion.Contains(IndexField.Text, "Hello"))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
      } yield {
        assertTrue(pageCount1 == 1) &&
        assertTrue(pageCount2 == 3) &&
        assertTrue(refsWorld == Seq(docRef1)) &&
        assertTrue(refsHiWorld == Seq(docRef1)) &&
        assertTrue(refsHello.toSet == Set(docRef1, docRef2)) &&
        assertTrue(
          textWithHtml.replaceAll("<(.+?)>", "") == f"${metadata2.title.get}${text2.replaceAll("\n", "")}"
        ) &&
        assertTrue(
          // Remove all HTML except for highlights marked with <b>word</b>
          textWithHtmlHighlights
            .replaceAll("<((?!b\\b|\\/b\\b).+?)>", "") == f"${metadata2.title.get}${text2.replaceAll("\n", "").replaceAll("([Tt]hink)", "<b>$1</b>")}"
        ) &&
        assertTrue(refsHelloAfterRemove == Seq(docRef2))
      }
    },
    test("search single occurrence without padding") {
      val startTime = Instant.now()
      for {
        searchRepo <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        resultAre <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "are")),
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
        assertTrue(
          resultAre.results.head.snippets.head.text == "<div class=\"text-snippet\">How <b>are</b> you?</div>"
        ) &&
        assertTrue(queries.head.query.contains("are"))
      }
    },
    test("list documents") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        docRefs <- searchService.list(SearchQuery(SearchCriterion.MatchAllDocuments))
      } yield {
        assertTrue(docRefs == Seq(docRef1, docRef2))
      }
    },
    test("search single occurrence with padding") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        resultArePadding <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "are")),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(1),
          "test",
          addOffsets = false
        )
        resultWithOffsets <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "are")),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(1),
          "test"
        )
      } yield {
        assertTrue(
          resultArePadding.results.head.snippets.head.text == "<div class=\"text-snippet\">Hello people.<br>" +
            "How <b>are</b> you?</div><div class=\"text-snippet\">" +
            "Fine, thank you.</div>"
        ) &&
        assertTrue(
          resultWithOffsets.results.head.snippets.head.text ==
            """<div class="text-snippet"><span offset="5">Hello people.</span><br>
              |<span offset="19">How </span><b><span offset="23">are</span></b><span offset="26"> you?</span></div><div class="text-snippet">
              |<span offset="33">Fine, thank you.</span></div>""".stripMargin.replaceAll("\n", "")
        )
      }
    },
    test("search single occurrence with hyphen") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        resultAre <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "today")),
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
          resultAre.results.head.snippets.head.text == "<div class=\"text-snippet\">Hello you.</div><div class=\"text-snippet\">" +
            "Nice day <b>to-</b><br>" +
            "<b>day</b>, Madam.<br>" +
            "Isn't it?</div>"
        )
      }
    },
    test("search phrase containing word with or without hyphen and wildcard") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        phraseResult <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"will rain tomorrow\"")),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(1),
          "test",
          addOffsets = false
        )
        phraseResultWithWildcard <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"will * tomorrow\"")),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(1),
          "test",
          addOffsets = false
        )
        phraseWithHyphenResult <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"day today Madam\"")),
          Sort.Score,
          0,
          100,
          Some(1),
          Some(1),
          "test",
          addOffsets = false
        )
        phraseResultWithSlop <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"tomorrow rain\"~3")),
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
          phraseResult.results.head.snippets.head.text == "<div class=\"text-snippet\">Think it <b>will</b> <b>rain</b><br>" +
            "<b>tomorrow</b>? Oh no, I<br>" +
            "don't think so.</div>"
        ) &&
        assertTrue(
          phraseResultWithWildcard.results.head.snippets.head.text == "<div class=\"text-snippet\">Think it <b>will</b> rain<br>" +
            "<b>tomorrow</b>? Oh no, I<br>" +
            "don't think so.</div>"
        ) &&
        assertTrue(
          phraseResult.results.head.snippets.head.page == 1
        ) &&
        assertTrue(
          phraseWithHyphenResult.results.head.snippets.head.text == "<div class=\"text-snippet\">Hello you.</div><div class=\"text-snippet\">" +
            "Nice <b>day</b> <b>to-</b><br>" +
            "<b>day</b>, <b>Madam</b>.<br>" +
            "Isn't it?</div>"
        ) &&
        assertTrue(
          phraseWithHyphenResult.results.head.snippets.head.page == 0
        ) &&
        assertTrue(
          phraseResultWithSlop.results.head.snippets.head.text == "<div class=\"text-snippet\">Think it will <b>rain</b><br>" +
            "<b>tomorrow</b>? Oh no, I<br>" +
            "don't think so.</div>"
        )
      }
    },
    test("search phrase wildcard should only highlight phrase if wildcard is matched") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef4, username, ipAddress, alto4, metadata4)
        phraseResult <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"With * pleasure\"")),
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
          phraseResult.results.head.snippets.head.text == "<div class=\"text-snippet\">With pleasure.<br>" +
            "<b>With</b> great <b>pleasure</b>.<br>" +
            "Really.</div>"
        )
      }
    },
    test("search with wildcard in word") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef4, username, ipAddress, alto4, metadata4)
        phraseResult <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "plea*")),
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
          phraseResult.results.head.snippets.head.text == "<div class=\"text-snippet\">With <b>pleasure</b>.<br>" +
            "With great <b>pleasure</b>.<br>" +
            "Really.</div>"
        )
      }
    },
    test("search with wildcard in phrase") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef4, username, ipAddress, alto4, metadata4)
        phraseResult <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"With gr* pleasure\"")),
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
          phraseResult.results.head.snippets.head.text == "<div class=\"text-snippet\">With pleasure.<br>" +
            "<b>With</b> <b>great</b> <b>pleasure</b>.<br>" +
            "Really.</div>"
        )
      }
    },
    test("correctly highlight synonyms if both are matched") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef3, username, ipAddress, alto3, metadata3)
        phraseResult <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "hello hi")),
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
          phraseResult.results.head.snippets.head.text == "<div class=\"text-snippet\"><b>Hello</b> everyone</div>"
        )
      }
    },
    test("search multiple occurrence without padding") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        resultsThink <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "think")),
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
            "<div class=\"text-snippet\"><b>Think</b> it will rain</div>",
            "<div class=\"text-snippet\">don't <b>think</b> so.</div>",
            "<div class=\"text-snippet\">I <b>think</b> it will be</div>"
          )
        )
      }
    },
    test("search multiple occurrence with padding") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        resultsThinkPadding <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "think")),
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
            "<div class=\"text-snippet\"><b>Think</b> it will rain<br>" +
              "tomorrow? Oh no, I<br>" +
              "don't <b>think</b> so.</div>",
            "<div class=\"text-snippet\">I <b>think</b> it will be<br>" +
              "sunny tomorrow, and even</div>"
          )
        )
      }
    },
    test("various search criteria") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        _ <- searchService.addFakeDocument(docRef3, username, ipAddress, alto3, metadata3)
        titleContainsWorld <- searchService.search(
          SearchQuery(SearchCriterion.Contains(Seq(IndexField.Title, IndexField.TitleEnglish), "world", strict = false))
        )
        titleContainsHelloWorld <- searchService.search(
          SearchQuery(
            SearchCriterion.Contains(Seq(IndexField.Title, IndexField.TitleEnglish), "\"hello world\"", strict = false)
          )
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
        titleAndTextCombined <- searchService.search(
          SearchQuery(
            SearchCriterion.And(
              SearchCriterion.Contains(
                Seq(IndexField.Title, IndexField.TitleEnglish),
                "\"hello world\"",
                strict = false
              ),
              SearchCriterion.Contains(IndexField.Text, "\"hello world\"", strict = false)
            )
          )
        )
      } yield {
        assertTrue(titleContainsWorld.results.map(_.docRef).sortBy(_.ref) == Seq(docRef1)) &&
        assertTrue(titleContainsHelloWorld.results.map(_.docRef).sortBy(_.ref) == Seq(docRef1)) &&
        assertTrue(authorInJoe.results.map(_.docRef).sortBy(_.ref) == Seq(docRef1, docRef3)) &&
        assertTrue(authorStartsWithJo.results.map(_.docRef).sortBy(_.ref) == Seq(docRef1, docRef3)) &&
        assertTrue(yearBefore1920.results.map(_.docRef).sortBy(_.ref) == Seq(docRef1, docRef2)) &&
        assertTrue(yearAfter1920.results.map(_.docRef).sortBy(_.ref) == Seq(docRef2, docRef3)) &&
        assertTrue(notAuthorInJoe.results.map(_.docRef).sortBy(_.ref) == Seq(docRef2)) &&
        assertTrue(andYear.results.map(_.docRef).sortBy(_.ref) == Seq(docRef2)) &&
        assertTrue(titleAndTextCombined.results.map(_.docRef).sortBy(_.ref) == Seq(docRef1))
      }
    },
    test("aggregate") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        _ <- searchService.addFakeDocument(docRef3, username, ipAddress, alto3, metadata3)
        binsHello <- searchService.aggregate(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "Hello")),
          IndexField.Author,
          Some(2)
        )
        binsHello1 <- searchService.aggregate(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "Hello")),
          IndexField.Author,
          Some(1)
        )
        binsHelloByLabel <- searchService.aggregate(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "Hello")),
          IndexField.Author,
          Some(2),
          sortByLabel = true
        )
      } yield {
        assertTrue(
          binsHello == AggregationBins(Seq(AggregationBin("Joe Schmoe", 2), AggregationBin("Jack Sprat", 1)))
        ) &&
        assertTrue(binsHello1 == AggregationBins(Seq(AggregationBin("Joe Schmoe", 2)))) &&
        assertTrue(
          binsHelloByLabel == AggregationBins(Seq(AggregationBin("Jack Sprat", 1), AggregationBin("Joe Schmoe", 2)))
        )
      }
    },
    test("return top authors in alphabetical order") {
      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.addFakeDocument(docRef1, username, ipAddress, alto1, metadata1)
        _ <- searchService.addFakeDocument(docRef2, username, ipAddress, alto2, metadata2)
        _ <- searchService.addFakeDocument(docRef3, username, ipAddress, alto3, metadata3)
        binsJ <- searchService.getTopAuthors(
          "J",
          Some(5),
          includeAuthorField = true,
          includeAuthorInTranscriptionField = true
        )
        binsJo <- searchService.getTopAuthors(
          "Jo",
          None,
          includeAuthorField = true,
          includeAuthorInTranscriptionField = true
        )
        binsJ1 <- searchService.getTopAuthors(
          "J",
          Some(1),
          includeAuthorField = true,
          includeAuthorInTranscriptionField = true
        )
        binsDalet <- searchService.getTopAuthors(
          "ד",
          Some(5),
          includeAuthorField = true,
          includeAuthorInTranscriptionField = true
        )
      } yield {
        assertTrue(binsJ == AggregationBins(Seq(AggregationBin("Jack Sprat", 1), AggregationBin("Joe Schmoe", 2)))) &&
        assertTrue(binsJo == AggregationBins(Seq(AggregationBin("Joe Schmoe", 2)))) &&
        assertTrue(binsJ1 == AggregationBins(Seq(AggregationBin("Joe Schmoe", 2)))) &&
        assertTrue(binsDalet == AggregationBins(Seq(AggregationBin("דשאָו שמאָו", 1))))
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ suggestionRepoLayer ++ indexLayer ++ languageSpecificFilterLayer) >>> SearchService.live ++ ZLayer
      .service[SearchRepo] ++ ZLayer
      .service[SuggestionRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
