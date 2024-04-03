package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.model.{Page, Word}
import com.joliciel.jochre.search.core.DocReference
import zio.Scope
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import java.time.Instant

object SearchRepoTest extends JUnitRunnableSpec with DatabaseTestBase {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("SearchRepoTest")(
    test("insert document") {
      val docRef = DocReference("doc1")
      val startTime = Instant.now()
      for {
        searchRepo <- getSearchRepo()
        docRev <- searchRepo.insertDocument(docRef)
        doc <- searchRepo.getDocument(docRef)
        doc2 <- searchRepo.getDocument(docRev)
      } yield {
        assertTrue(doc.rev == docRev) &&
        assertTrue(doc.ref == docRef) &&
        assertTrue(doc.created.toEpochMilli > startTime.toEpochMilli) &&
        assertTrue(doc == doc2)
      }
    },
    test("insert page") {
      val docRef = DocReference("doc1")
      val page = Page("page_1", 200, 100, 42, 0.17, "yi", 0.9, Seq.empty)
      for {
        searchRepo <- getSearchRepo()
        docRev <- searchRepo.insertDocument(docRef)
        pageId <- searchRepo.insertPage(docRev, page)
        page <- searchRepo.getPage(docRef, 42).map(_.get)
        page2 <- searchRepo.getPage(pageId)
      } yield {
        assertTrue(page.id == pageId) &&
        assertTrue(page.docRev == docRev) &&
        assertTrue(page.width == 100) &&
        assertTrue(page.height == 200) &&
        assertTrue(page.index == 42) &&
        assertTrue(page == page2)
      }
    },
    test("insert row") {
      val docRef = DocReference("doc1")
      val page = Page("page_1", 200, 100, 42, 0.17, "yi", 0.9, Seq.empty)
      val rectangle = Rectangle(10, 13, 90, 20)
      for {
        searchRepo <- getSearchRepo()
        docRev <- searchRepo.insertDocument(docRef)
        pageId <- searchRepo.insertPage(docRev, page)
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
      val page = Page("page_1", 200, 100, 42, 0.17, "yi", 0.9, Seq.empty)
      val rectangle = Rectangle(10, 13, 90, 20)
      val word = Word("hello", Rectangle(20, 13, 40, 20), Seq.empty, Seq.empty, 0.80)
      for {
        searchRepo <- getSearchRepo()
        docRev <- searchRepo.insertDocument(docRef)
        pageId <- searchRepo.insertPage(docRev, page)
        rowId <- searchRepo.insertRow(pageId, 12, rectangle)
        wordId <- searchRepo.insertWord(docRev, rowId, 10, None, word)
        dbWord <- searchRepo.getWord(docRev, 10).map(_.get)
        dbWord2 <- searchRepo.getWord(wordId)
        _ <- searchRepo.insertWord(docRev, rowId, 20, Some(26), word)
        hyphenatedWord <- searchRepo.getWord(docRev, 20).map(_.get)
      } yield {
        assertTrue(dbWord.id == wordId) &&
        assertTrue(dbWord.docRev == docRev) &&
        assertTrue(dbWord.rowId == rowId) &&
        assertTrue(dbWord.left == 20) &&
        assertTrue(dbWord.top == 13) &&
        assertTrue(dbWord.width == 40) &&
        assertTrue(dbWord.height == 20) &&
        assertTrue(dbWord.offset == 10) &&
        assertTrue(dbWord.hyphenatedOffset == None) &&
        assertTrue(dbWord == dbWord2) &&
        assertTrue(hyphenatedWord.hyphenatedOffset == Some(26))
      }
    }
  ).provideLayer(searchRepoLayer) @@ TestAspect.sequential
}
