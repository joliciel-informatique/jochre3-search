package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.ocr.core.model.SpellingAlternative
import com.joliciel.jochre.search.core.DocReference
import com.joliciel.jochre.search.core.lucene.{DocumentIndexInfo, IndexingHelper, LuceneUtilities, Token}
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.apache.lucene.analysis.standard.StandardTokenizer
import org.apache.lucene.analysis.{Analyzer, TokenStream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.Locale

class HyphenationFilterTest extends AnyFlatSpec with Matchers with LuceneUtilities {
  class TestAnalyzer(indexingHelper: IndexingHelper) extends Analyzer {
    override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
      val source = new WhitespaceTokenizer();

      new TokenStreamComponents(source, finalFilter(source))
    }

    def finalFilter(tokens: TokenStream): TokenStream = {
      val addRefFilter = new AddDocumentReferenceFilter(tokens, indexingHelper)
      val regexTokenizer = new RegexTokenizerFilter(addRefFilter, raw"""\p{Punct}""".r)
      new HyphenationFilter(regexTokenizer, indexingHelper)
    }
  }

  "HyphenationFilter" should "should replace word with hyphenated version at the right offset" in {
    val indexingHelper = IndexingHelper()
    val analyzer = new TestAnalyzer(indexingHelper)
    val docRef = DocReference("docRef")

    val text =
      f"${docRef.ref}\ndog to-\nday cat"

    indexingHelper.addDocumentInfo(
      docRef,
      DocumentIndexInfo(
        pageOffsets = Set.empty,
        newlineOffsets = Set.empty,
        hyphenatedWordOffsets = Set(f"${docRef.ref}\ndog ".length),
        offsetToAlternativeMap = Map.empty
      )
    )

    val tokens = tokenizeString(text, analyzer)

    val expected = Seq(
      Token("dog", f"${docRef.ref}\n".length, f"${docRef.ref}\ndog".length, 1.0f),
      Token("today", f"${docRef.ref}\ndog ".length, f"${docRef.ref}\ndog to-\nday".length, 1.0f),
      Token("cat", f"${docRef.ref}\ndog to-\nday ".length, f"${docRef.ref}\ndog to-\nday cat".length, 1.0f)
    )

    tokens should equal(expected)
  }
}
