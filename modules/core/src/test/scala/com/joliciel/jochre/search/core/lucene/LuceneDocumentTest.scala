package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.service.{DocRev, Highlight, HighlightedPage}
import com.joliciel.jochre.search.core.{AltoDocument, DocMetadata, DocReference, IndexField, WithTestIndex}
import org.apache.lucene.index.Term
import org.apache.lucene.search.TermQuery
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Using

class LuceneDocumentTest extends AnyFlatSpec with Matchers with LuceneUtilities with WithTestIndex {
  private val docRef = DocReference("doc1")
  private val prefix = f"${docRef.ref}\n"
  private val page1line1 = "Hello cat.\n"
  private val page1line2 = "Is the c-\n"
  private val page1line3 = "at happy? "
  private val page1 = page1line1 + page1line2 + page1line3
  private val page2line1 = "Yes, the cat is happy.\n"
  private val page2line2 = "Oh good. "
  private val page2 = page2line1 + page2line2
  private val page3 = "Life is good."
  private val doc = AltoDocument(
    docRef,
    DocRev(42),
    f"$prefix$page1$page2$page3",
    DocMetadata(title = Some("The cat"))
  )

  "highlightPagesAsHtml" should "correctly highlight pages" in {
    val jochreIndex = getJochreIndex

    jochreIndex.addDocumentInfo(
      docRef,
      DocumentIndexInfo(
        pageOffsets = Set(prefix.length, prefix.length + page1.length, prefix.length + page1.length + page2.length),
        newlineOffsets = Set(
          prefix.length + page1line1.length,
          prefix.length + page1line1.length + page1line2.length,
          prefix.length + page1.length + page2line1.length
        ),
        hyphenatedWordOffsets = Set(prefix.length + page1line1.length + "Is the ".length),
        offsetToAlternativeMap = Map.empty
      )
    )
    jochreIndex.indexer.indexDocument(doc)
    jochreIndex.refresh

    Using.resource(jochreIndex.searcherManager.acquire()) { jochreSearcher =>
      val luceneDocOpt = jochreSearcher.getByDocRef(docRef)
      assert(luceneDocOpt.isDefined)
      val luceneDoc = luceneDocOpt.get
      val query = new TermQuery(new Term(IndexField.Text.entryName, "cat"))
      val highlightedPages = luceneDoc.highlightPagesAsHtml(query)
      highlightedPages shouldEqual Seq(
        prefix.length -> "Hello <b>cat</b>.<br>Is the <b>c-</b><br><b>at</b> happy? ",
        prefix.length + page1.length -> "Yes, the <b>cat</b> is happy.<br>Oh good. ",
        prefix.length + page1.length + page2.length -> "Life is good."
      )
    }
  }

  "higlightPages" should "correctly highlight pages" in {
    val jochreIndex = getJochreIndex

    jochreIndex.addDocumentInfo(
      docRef,
      DocumentIndexInfo(
        pageOffsets = Set(prefix.length, prefix.length + page1.length, prefix.length + page1.length + page2.length),
        newlineOffsets = Set(
          prefix.length + page1line1.length,
          prefix.length + page1line1.length + page1line2.length,
          prefix.length + page1.length + page2line1.length
        ),
        hyphenatedWordOffsets = Set(prefix.length + page1line1.length + "Is the ".length),
        offsetToAlternativeMap = Map.empty
      )
    )
    jochreIndex.indexer.indexDocument(doc)
    jochreIndex.refresh

    Using.resource(jochreIndex.searcherManager.acquire()) { jochreSearcher =>
      val luceneDocOpt = jochreSearcher.getByDocRef(docRef)
      assert(luceneDocOpt.isDefined)
      val luceneDoc = luceneDocOpt.get
      val query = new TermQuery(new Term(IndexField.Text.entryName, "cat"))
      val highlightedPages = luceneDoc.highlightPages(query)
      highlightedPages shouldEqual Seq(
        HighlightedPage(
          0,
          prefix.length,
          "Hello cat.\nIs the c-\nat happy? ",
          Seq(
            Highlight("Hello ".length, "Hello cat".length),
            Highlight("Hello cat.\nIs the ".length, "Hello cat.\nIs the c-\nat".length)
          )
        ),
        HighlightedPage(
          0,
          prefix.length + page1.length,
          "Yes, the cat is happy.\nOh good. ",
          Seq(
            Highlight("Yes, the ".length, "Yes, the cat".length)
          )
        ),
        HighlightedPage(0, prefix.length + page1.length + page2.length, "Life is good.", Seq.empty[Highlight])
      )
    }
  }
}