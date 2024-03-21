package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.DocReference
import org.apache.lucene.index.{LeafReaderContext, PostingsEnum, ReaderUtil, Term, TermsEnum}
import org.apache.lucene.search.DocIdSetIterator
import org.apache.lucene.util.BytesRef
import org.slf4j.LoggerFactory

import scala.util.Using
import scala.jdk.CollectionConverters._

case class TermLister(searcherManager: JochreSearcherManager) {
  private val log = LoggerFactory.getLogger(getClass)

  def listTerms(docRef: DocReference): Map[String, Seq[IndexTerm]] = {
    Using(searcherManager.acquire()) { searcher =>
      val luceneDoc = searcher
        .getByDocRef(docRef)
        .getOrElse(throw new Exception(f"Document not found in index for ${docRef.ref}"))

      val readerContext = searcher.getIndexReader.getContext
      val leaves = readerContext.leaves
      val leaf = ReaderUtil.subIndex(luceneDoc.luceneId, leaves)

      val subContext = leaves.get(leaf)
      val atomicReader = subContext.reader
      val fields = atomicReader.getFieldInfos.asScala.map(_.name)

      val fieldToTermMap = fields
        .map { field =>
          val atomicReaderTerms = Option(atomicReader.terms(field))
          val terms = atomicReaderTerms
            .map { atomicReaderTerms =>
              val termsEnum = atomicReaderTerms.iterator()
              Iterator
                .continually(Option(termsEnum.next()))
                .takeWhile(_.isDefined)
                .flatMap { _ =>
                  findTerms(field, termsEnum, subContext, luceneDoc.luceneId)
                }
                .toSeq
            }
            .getOrElse {
              log.debug(f"No terms for document ${docRef.ref}, field ${field}")
              Seq.empty
            }
          val sortedTerms = terms.sortBy(term => (term.start, term.end, term.text))
          field -> sortedTerms
        }
        .filter(!_._2.isEmpty)
        .toMap

      fieldToTermMap
    }.get
  }

  private def findTerms(
      field: String,
      termsEnum: TermsEnum,
      subContext: LeafReaderContext,
      luceneId: Int
  ): Seq[IndexTerm] = {
    val term: Term = new Term(field, BytesRef.deepCopyOf(termsEnum.term))
    val docPosEnum: PostingsEnum =
      termsEnum.postings(null, PostingsEnum.OFFSETS | PostingsEnum.POSITIONS | PostingsEnum.PAYLOADS)
    Iterator
      .continually(docPosEnum.nextDoc)
      .takeWhile { nextDoc => nextDoc != DocIdSetIterator.NO_MORE_DOCS }
      .flatMap { relativeId =>
        val nextId: Int = subContext.docBase + relativeId
        if (luceneId == nextId) {
          // Retrieve the term frequency in the current document
          val freq: Int = docPosEnum.freq
          if (log.isTraceEnabled)
            log.trace("Found " + freq + " matches for term " + term + ", luceneId " + nextId + ", field " + field)
          (0 until freq).map { _ =>
            val position: Int = docPosEnum.nextPosition
            val start: Int = docPosEnum.startOffset
            val end: Int = docPosEnum.endOffset
            if (log.isTraceEnabled)
              log.trace(
                "Found match " + position + " at luceneId " + nextId + ", field " + field + " start=" + start + ", end=" + end
              )
            IndexTerm(term.text, start, end, position)
          }
        } else {
          Seq.empty
        }
      }
      .toSeq
  }
}
