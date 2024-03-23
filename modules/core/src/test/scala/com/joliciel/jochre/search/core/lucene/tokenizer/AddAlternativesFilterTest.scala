package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.ocr.core.model.SpellingAlternative
import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.{DocumentIndexInfo, IndexingHelper, LuceneUtilities, Token}
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.{Analyzer, TokenStream}
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

class AddAlternativesFilterTest extends AnyFlatSpec with Matchers with LuceneUtilities {
  class TestAnalyzer(indexingHelper: IndexingHelper) extends Analyzer {
    override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
      val source = new WhitespaceTokenizer();

      new TokenStreamComponents(source, finalFilter(source))
    }

    def finalFilter(tokens: TokenStream): TokenStream = {
      val addRefFilter = new AddDocumentReferenceFilter(tokens, indexingHelper)
      new AddAlternativesFilter(addRefFilter, indexingHelper)
    }
  }

  "AddAlternativesFilter" should "add alternatives at the right offset" in {
    val indexingHelper = IndexingHelper()
    val analyzer = new TestAnalyzer(indexingHelper)
    val docRef = DocReference("docRef")

    val text =
      f"${docRef.ref}\ndog mouse cat"

    indexingHelper.addDocumentInfo(
      docRef,
      DocumentIndexInfo(
        pageOffsets = Set.empty,
        newlineOffsets = Set.empty,
        offsetToAlternativeMap = Map(
          f"${docRef.ref}\n".length -> Seq(
            SpellingAlternative("hyponym", "bulldog"),
            SpellingAlternative("hyponym", "greyhound")
          ),
          f"${docRef.ref}\ndog mouse ".length -> Seq(SpellingAlternative("synonym", "kitty"))
        )
      )
    )

    val tokens = tokenizeString(text, analyzer)

    val expected = Seq(
      Token("dog", f"${docRef.ref}\n".length, f"${docRef.ref}\ndog".length, 1.0f),
      Token("bulldog", f"${docRef.ref}\n".length, f"${docRef.ref}\ndog".length, 1.0f),
      Token("greyhound", f"${docRef.ref}\n".length, f"${docRef.ref}\ndog".length, 1.0f),
      Token("mouse", f"${docRef.ref}\ndog ".length, f"${docRef.ref}\ndog mouse".length, 1.0f),
      Token("cat", f"${docRef.ref}\ndog mouse ".length, f"${docRef.ref}\ndog mouse cat".length, 1.0f),
      Token("kitty", f"${docRef.ref}\ndog mouse ".length, f"${docRef.ref}\ndog mouse cat".length, 1.0f)
    )

    tokens should equal(expected)
  }
}
