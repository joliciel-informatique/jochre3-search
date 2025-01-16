package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.service.{DocRev, Highlight, HighlightedPage}
import com.joliciel.jochre.search.core.{AltoDocument, DocMetadata, DocReference, IndexField, WithTestIndex}
import org.apache.lucene.index.Term
import org.apache.lucene.search.TermQuery
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Using
import com.joliciel.jochre.search.core.SearchCriterion
import com.joliciel.jochre.search.core.SearchQuery
import org.slf4j.LoggerFactory
import com.joliciel.jochre.search.core.WithYiddishTestIndex

class LuceneDocumentYiddishTest extends AnyFlatSpec with Matchers with LuceneUtilities with WithYiddishTestIndex {
  private val log = LoggerFactory.getLogger(getClass)

  private val docRef = DocReference("doc1")
  private val prefix = f"${docRef.ref}\n"
  private val page1line1 = "דאָ מוז ער נאָך מיט אים רײדן אױפן אײדעלן װעג, װי מיט אַ העכערן.\n"
  private val page1 = page1line1
  private val doc = AltoDocument(
    docRef,
    DocRev(42),
    f"$prefix$page1",
    DocMetadata(title = Some("ניו־יאָרקער נאָװעלן"))
  )

  "highlight" should "correctly highlight a document and not throw TooManyClauses on query with a large number of synonyms" in {
    val jochreIndex = getJochreIndex

    jochreIndex.addDocumentInfo(
      docRef,
      DocumentIndexInfo(
        pageOffsets = Set(prefix.length),
        newlineOffsets = Set(prefix.length),
        hyphenatedWordOffsets = Set.empty,
        offsetToAlternativeMap = Map.empty
      )
    )
    jochreIndex.indexer.indexDocument(doc)
    jochreIndex.refresh

    val query = SearchQuery(SearchCriterion.Contains(IndexField.Text, "\"אײדעלן װעג\"", strict = false))
    val luceneQuery = query.criterion.toLuceneQuery(analyzerGroup)

    Using.resource(jochreIndex.searcherManager.acquire()) { jochreSearcher =>
      val luceneDocOpt = jochreSearcher.getByDocRef(docRef)
      assert(luceneDocOpt.isDefined)
      val luceneDoc = luceneDocOpt.get
      val snippets = luceneDoc.highlight(luceneQuery, addOffsets = false)
      val texts = snippets.map(_.text)
      texts shouldEqual Seq(
        "דאָ מוז ער נאָך מיט אים רײדן אױפן <b>אײדעלן</b> <b>װעג</b>, װי מיט אַ העכערן."
      )
    }
  }
}
