package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.{DocReference, IndexField, MetadataField, SearchCriterion, SearchQuery, Sort}
import com.joliciel.jochre.search.yiddish.YiddishFilters
import org.slf4j.LoggerFactory
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, ZIO, ZLayer}

import java.io.File
import javax.imageio.ImageIO
import com.joliciel.jochre.search.core.WithYiddishTestIndex

object SearchServiceYiddishTest
    extends JUnitRunnableSpec
    with DatabaseTestBase
    with WithYiddishTestIndexLayer
    with WithDehyphenatorLayer
    with AltoHelper {
  private val log = LoggerFactory.getLogger(getClass)

  private val username = "jimi@hendrix.org"
  private val ipAddress = Some("127.0.0.1")

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SearchServiceYiddishTest")(
    test("upload real pdf and get image snippets") {
      val docRef = DocReference("nybc200089")
      val pdfStream = getClass.getResourceAsStream("/nybc200089-11-12.pdf")

      val altoStream = getClass.getResourceAsStream("/nybc200089-11-12_alto4.zip")

      val metadataStream = getClass.getResourceAsStream("/nybc200089_meta.xml")

      for {
        _ <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        pageCount <- searchService.addNewDocumentAsPdf(
          docRef,
          username,
          ipAddress,
          pdfStream,
          altoStream,
          Some(metadataStream)
        )
        searchResults <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "farshvundn")),
          Sort.Score,
          0,
          10,
          Some(20),
          Some(1),
          username,
          ipAddress,
          addOffsets = false
        )
        topResult <- ZIO.attempt(searchResults.results.head)
        prefixResults <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "פֿאַרשװ*")),
          Sort.Score,
          0,
          10,
          Some(20),
          Some(1),
          username,
          ipAddress,
          addOffsets = false
        )
        topPrefixResult <- ZIO.attempt(prefixResults.results.head)
        imageSnippet <- searchService.getImageSnippet(
          topResult.docRef,
          topResult.snippets.head.start,
          topResult.snippets.head.end,
          topResult.snippets.head.highlights
        )
        authorResultYiddish <- searchService.getTopAuthors(
          "של",
          Some(1),
          includeAuthorField = true,
          includeAuthorInTranscriptionField = true
        )
        authorResultYiddishNiqqud <- searchService.getTopAuthors(
          "שׁל",
          Some(1),
          includeAuthorField = true,
          includeAuthorInTranscriptionField = true
        )
        authorResultRomanized <- searchService.getTopAuthors(
          "Sh",
          Some(1),
          includeAuthorField = true,
          includeAuthorInTranscriptionField = true
        )
        authorResultRomanizedLowercase <- searchService.getTopAuthors(
          "sh",
          Some(1),
          includeAuthorField = true,
          includeAuthorInTranscriptionField = true
        )
        transliteratedTitleResults <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.TitleEnglish, "verk")),
          Sort.Score,
          0,
          10,
          Some(20),
          Some(1),
          username,
          ipAddress,
          addOffsets = false
        )
        titleResults <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Title, "\"ale verk\"")),
          Sort.Score,
          0,
          10,
          Some(20),
          Some(1),
          username,
          ipAddress,
          addOffsets = false
        )
        _ <- searchService.removeDocument(docRef)
      } yield {
        val tempFile = File.createTempFile("jochre-snippet", ".png")
        ImageIO.write(imageSnippet, "png", tempFile)
        log.info(f"Wrote snippet to ${tempFile.getPath}")
        assertTrue(searchResults.totalCount == 1) &&
        assertTrue(searchResults.results.head.metadata.collections == Seq("nationalyiddishbookcenter")) &&
        assertTrue(pageCount == 2) &&
        assertTrue(
          topResult.snippets.head.text == "<div class=\"text-snippet\">דאָרט װאו די שיטערע רױכיגע װאָלקענס שװעבען, דאָרט װאו<br>" +
            "די װײסע פױנלען טוקען זיך, באַװײזען זיך און װערען <b>פאַרשװאונ־</b><br>" +
            "<b>דען</b> מיט אַ קװיטש און מיט אַ צװיטשער, און עס רײסט זיך<br>" +
            "אַרױס פון מײן אָנגעפילטער ברוסט, אָהן מײן װיסען, אַ מין גע־</div>"
        ) &&
        assertTrue(
          topPrefixResult.snippets.head.text == "<div class=\"text-snippet\">דאָרט װאו די שיטערע רױכיגע װאָלקענס שװעבען, דאָרט װאו<br>" +
            "די װײסע פױנלען טוקען זיך, באַװײזען זיך און װערען <b>פאַרשװאונ־</b><br>" +
            "<b>דען</b> מיט אַ קװיטש און מיט אַ צװיטשער, און עס רײסט זיך<br>" +
            "אַרױס פון מײן אָנגעפילטער ברוסט, אָהן מײן װיסען, אַ מין גע־</div>"
        ) &&
        assertTrue(topResult.snippets.head.page == 2) &&
        assertTrue(topResult.metadata.titleEnglish == Some("Ale ṿerḳ"))
        assertTrue(authorResultYiddish.bins.size == 1) &&
        assertTrue(authorResultYiddish.bins(0).label == "שלום עליכם") &&
        assertTrue(authorResultYiddishNiqqud.bins.size == 1) &&
        assertTrue(authorResultYiddishNiqqud.bins(0).label == "שלום עליכם") &&
        assertTrue(authorResultRomanized.bins.size == 1) &&
        assertTrue(authorResultRomanized.bins(0).label == "Sholem Aleichem, 1859-1916") &&
        assertTrue(authorResultRomanizedLowercase.bins.size == 1) &&
        assertTrue(authorResultRomanizedLowercase.bins(0).label == "Sholem Aleichem, 1859-1916") &&
        assertTrue(transliteratedTitleResults.totalCount == 1) &&
        assertTrue(titleResults.totalCount == 1)
      }
    },
    test("upload real image zip and get image snippets as well as applying a metadata correction") {
      val docRef = DocReference("nybc200089")
      val imageZipStream = getClass.getResourceAsStream("/nybc200089-11-12.zip")

      val altoStream = getClass.getResourceAsStream("/nybc200089-11-12_alto4.zip")

      val metadataStream = getClass.getResourceAsStream("/nybc200089_meta.xml")

      for {
        searchRepo <- getSearchRepo()
        _ <- getSuggestionRepo()
        searchService <- ZIO.service[SearchService]
        pageCount <- searchService.addNewDocumentAsImages(
          docRef,
          username,
          ipAddress,
          imageZipStream,
          altoStream,
          Some(metadataStream)
        )
        _ <- searchService.correctMetadata(
          "test",
          None,
          docRef,
          MetadataField.AuthorEnglish,
          "Sholem Aleykhem",
          applyEverywhere = false
        )
        _ <- searchService.reindexWhereRequired()
        searchResults <- searchService.search(
          SearchQuery(SearchCriterion.Contains(IndexField.Text, "farshvundn")),
          Sort.Score,
          0,
          10,
          Some(20),
          Some(1),
          username,
          ipAddress,
          addOffsets = false
        )
        topResult <- ZIO.attempt(searchResults.results.head)
        imageSnippet <- searchService.getImageSnippet(
          topResult.docRef,
          topResult.snippets.head.start,
          topResult.snippets.head.end,
          topResult.snippets.head.highlights
        )
        indexedDoc <- searchRepo.getIndexedDocument(docRef)
        indexedDocCorrections <- searchRepo.getIndexedDocumentCorrections(docRef)
        _ <- searchService.removeDocument(docRef)
      } yield {
        val tempFile = File.createTempFile("jochre-snippet", ".png")
        ImageIO.write(imageSnippet, "png", tempFile)
        log.info(f"Wrote snippet to ${tempFile.getPath}")
        assertTrue(searchResults.totalCount == 1) &&
        assertTrue(pageCount == 2) &&
        assertTrue(
          topResult.snippets.head.text == "<div class=\"text-snippet\">דאָרט װאו די שיטערע רױכיגע װאָלקענס שװעבען, דאָרט װאו<br>" +
            "די װײסע פױנלען טוקען זיך, באַװײזען זיך און װערען <b>פאַרשװאונ־</b><br>" +
            "<b>דען</b> מיט אַ קװיטש און מיט אַ צװיטשער, און עס רײסט זיך<br>" +
            "אַרױס פון מײן אָנגעפילטער ברוסט, אָהן מײן װיסען, אַ מין גע־</div>"
        ) &&
        assertTrue(topResult.snippets.head.page == 2) &&
        assertTrue(topResult.metadata.authorEnglish.contains("Sholem Aleykhem")) &&
        assertTrue(indexedDoc.isDefined) &&
        assertTrue(indexedDocCorrections.nonEmpty)
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ suggestionRepoLayer ++ indexLayer ++ YiddishFilters.live ++ dehyphenatorLayer) >>> SearchService.live ++ ZLayer
      .service[SearchRepo] ++ ZLayer.service[SuggestionRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
