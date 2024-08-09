package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Page, Word}
import com.joliciel.jochre.search.core.{DocReference, IndexField, SearchCriterion, Sort}
import zio.Scope
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import java.time.Instant

object SearchRepoTest extends JUnitRunnableSpec with DatabaseTestBase {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("SearchRepoTest")(
    test("insert document") {
      val docRef = DocReference("doc1")
      val joe = "Joe"
      val joeIp = Some("127.0.0.1")
      val startTime = Instant.now()
      for {
        searchRepo <- getSearchRepo()
        docRev <- searchRepo.insertDocument(docRef, joe, joeIp)
        doc <- searchRepo.getDocument(docRef)
        doc2 <- searchRepo.getDocument(docRev)
        _ <- searchRepo.updateDocumentStatus(docRev, DocumentStatus.Complete)
        docsToReindex <- searchRepo.getDocumentsToReindex()
        indexedDoc1 <- searchRepo.getIndexedDocument(docRef)
        _ <- searchRepo.upsertIndexedDocument(
          docRef,
          docRev,
          Some(WordSuggestionRev(42)),
          Some(MetadataCorrectionRev(72)),
          true
        )
        indexedDoc2 <- searchRepo.getIndexedDocument(docRef)
      } yield {
        assertTrue(doc.rev == docRev) &&
        assertTrue(doc.ref == docRef) &&
        assertTrue(doc.username == joe) &&
        assertTrue(doc.ipAddress == joeIp) &&
        assertTrue(doc.created.toEpochMilli > startTime.toEpochMilli) &&
        assertTrue(doc == doc2) &&
        assertTrue(docsToReindex == Seq(docRef)) &&
        assertTrue(indexedDoc1.isDefined) &&
        assertTrue(indexedDoc1.get.docRev == DocRev(0)) &&
        assertTrue(!indexedDoc1.get.reindex) &&
        assertTrue(indexedDoc2.isDefined) &&
        assertTrue(indexedDoc2.get.wordSuggestionRev == Some(WordSuggestionRev(42))) &&
        assertTrue(indexedDoc2.get.metadataCorrectionRev == Some(MetadataCorrectionRev(72))) &&
        assertTrue(indexedDoc2.get.reindex) &&
        assertTrue(indexedDoc2.get.docRev == docRev)
      }
    },
    test("insert page") {
      val docRef = DocReference("doc1")
      val joe = "Joe"
      val joeIp = Some("127.0.0.1")
      val pageToInsert = Page("page_1", 200, 100, 42, 0.17, "yi", 0.9, Seq.empty)
      val page2ToInsert = Page("page_2", 200, 100, 43, 0.17, "yi", 0.9, Seq.empty)
      for {
        searchRepo <- getSearchRepo()
        docRev <- searchRepo.insertDocument(docRef, joe, joeIp)
        pageId <- searchRepo.insertPage(docRev, pageToInsert, 10)
        page <- searchRepo.getPage(docRev, 42).map(_.get)
        pageById <- searchRepo.getPage(pageId)
        page2Id <- searchRepo.insertPage(docRev, page2ToInsert, 20)
        pages <- searchRepo.getPages(docRev)
      } yield {
        assertTrue(page.id == pageId) &&
        assertTrue(page.docRev == docRev) &&
        assertTrue(page.width == 100) &&
        assertTrue(page.height == 200) &&
        assertTrue(page.offset == 10) &&
        assertTrue(page.index == 42) &&
        assertTrue(page == pageById) &&
        assertTrue(pages.map(_.id) == Seq(pageId, page2Id))
      }
    },
    test("insert row") {
      val docRef = DocReference("doc1")
      val joe = "Joe"
      val joeIp = Some("127.0.0.1")
      val page = Page("page_1", 200, 100, 42, 0.17, "yi", 0.9, Seq.empty)
      val rectangle = Rectangle(10, 13, 90, 20)
      for {
        searchRepo <- getSearchRepo()
        docRev <- searchRepo.insertDocument(docRef, joe, joeIp)
        pageId <- searchRepo.insertPage(docRev, page, 100)
        rowId <- searchRepo.insertRow(pageId, 12, rectangle)
        row <- searchRepo.getRow(docRev, 42, 12).map(_.get)
        row2 <- searchRepo.getRow(rowId)
      } yield {
        assertTrue(row.id == rowId) &&
        assertTrue(row.pageId == pageId) &&
        assertTrue(row.left == 10) &&
        assertTrue(row.top == 13) &&
        assertTrue(row.width == 90) &&
        assertTrue(row.height == 20) &&
        assertTrue(row.index == 12) &&
        assertTrue(row == row2)
      }
    },
    test("insert word") {
      val docRef = DocReference("doc1")
      val joe = "Joe"
      val joeIp = Some("127.0.0.1")
      val page = Page("page_1", 200, 100, 42, 0.17, "yi", 0.9, Seq.empty)
      val rectangle = Rectangle(10, 13, 90, 20)
      val word = Word("hello", Rectangle(20, 13, 40, 20), Seq.empty, Seq.empty, 0.80)
      val word2 = Word("uni-", Rectangle(70, 13, 100, 20), Seq.empty, Seq.empty, 0.90)
      for {
        searchRepo <- getSearchRepo()
        docRev <- searchRepo.insertDocument(docRef, joe, joeIp)
        pageId <- searchRepo.insertPage(docRev, page, 10)
        rowId <- searchRepo.insertRow(pageId, 12, rectangle)
        wordId <- searchRepo.insertWord(docRev, rowId, 10, None, word)
        dbWord <- searchRepo.getWord(docRev, 10).map(_.get)
        dbWordById <- searchRepo.getWord(wordId)
        hyphenatedWordId <- searchRepo.insertWord(docRev, rowId, 20, Some(26), word2)
        hyphenatedWord <- searchRepo.getWord(docRev, 20).map(_.get)
        wordPage <- searchRepo.getPageByWordOffset(docRev, 10)
        wordsInRow <- searchRepo.getWordsInRow(docRev, 10)
        _ <- searchRepo.deleteDocument(docRev)
        dbWordAfterDelete <- searchRepo.getWord(docRev, 10)
      } yield {
        assertTrue(dbWord.id == wordId) &&
        assertTrue(dbWord.docRev == docRev) &&
        assertTrue(dbWord.rowId == rowId) &&
        assertTrue(dbWord.left == 20) &&
        assertTrue(dbWord.top == 13) &&
        assertTrue(dbWord.width == 40) &&
        assertTrue(dbWord.height == 20) &&
        assertTrue(dbWord.startOffset == 10) &&
        assertTrue(dbWord.hyphenatedOffset.isEmpty) &&
        assertTrue(dbWord == dbWordById) &&
        assertTrue(hyphenatedWord.hyphenatedOffset.contains(26)) &&
        assertTrue(wordPage.map(_.index).contains(42)) &&
        assertTrue(wordsInRow.map(_.id) == Seq(wordId, hyphenatedWordId)) &&
        assertTrue(dbWordAfterDelete.isEmpty)
      }
    },
    test("retrieve sequential rows by offset") {
      val docRef = DocReference("doc1")
      val joe = "Joe"
      val joeIp = Some("127.0.0.1")
      val page = Page("page_1", 200, 100, 42, 0.17, "yi", 0.9, Seq.empty)
      val rect1 = Rectangle(10, 10, 90, 20)
      val word1 = Word("hello", Rectangle(20, 10, 40, 20), Seq.empty, Seq.empty, 0.80)
      val rect2 = Rectangle(5, 20, 130, 30)
      val rect3 = Rectangle(15, 30, 150, 40)
      val word3 = Word("word", Rectangle(15, 30, 50, 40), Seq.empty, Seq.empty, 0.90)
      for {
        searchRepo <- getSearchRepo()
        docRev <- searchRepo.insertDocument(docRef, joe, joeIp)
        pageId <- searchRepo.insertPage(docRev, page, 10)
        rowId1 <- searchRepo.insertRow(pageId, 12, rect1)
        _ <- searchRepo.insertWord(docRev, rowId1, 10, None, word1)
        rowId2 <- searchRepo.insertRow(pageId, 13, rect2)
        rowId3 <- searchRepo.insertRow(pageId, 14, rect3)
        _ <- searchRepo.insertWord(docRev, rowId3, 40, None, word3)
        dbRow1 <- searchRepo.getRowByStartOffset(docRev, 10)
        dbRow3 <- searchRepo.getRowByEndOffset(docRev, 45)
        rows <- searchRepo.getRowsByStartAndEndOffset(docRev, 10, 45)
      } yield {
        assertTrue(dbRow1.map(_.id).contains(rowId1)) &&
        assertTrue(dbRow3.map(_.id).contains(rowId3)) &&
        assertTrue(rows.map(_.id) == Seq(rowId1, rowId2, rowId3))
      }
    },
    test("insert query") {
      val startTime = Instant.now()
      val joe = "Joe"
      val joeIp = Some("127.0.0.1")
      val criteria = SearchCriterion.And(
        SearchCriterion.Contains(IndexField.Text, "Hello"),
        SearchCriterion.Not(SearchCriterion.ValueIn(IndexField.Author, Seq("Joe Schmoe")))
      )
      val sort = Sort.Field(IndexField.PublicationYearAsNumber, ascending = true)
      for {
        searchRepo <- getSearchRepo()
        queryId <- searchRepo.insertQuery(joe, joeIp, criteria, sort, 20, 42, 72)
        query <- searchRepo.getQuery(queryId)
        queries <- searchRepo.getQueriesSince(startTime)
      } yield {
        assertTrue(query.criteria == criteria) &&
        assertTrue(query.sort == sort) &&
        assertTrue(query.username == joe) &&
        assertTrue(query.ipAddress == joeIp) &&
        assertTrue(query.first == 20) &&
        assertTrue(query.max == 42) &&
        assertTrue(query.resultCount == 72) &&
        assertTrue(query.executed.toEpochMilli > startTime.toEpochMilli) &&
        assertTrue(query.query.contains("Hello")) &&
        assertTrue(queries.nonEmpty) &&
        assertTrue(queries.head.id == queryId)
      }
    }
  ).provideLayer(searchRepoLayer) @@ TestAspect.sequential
}
