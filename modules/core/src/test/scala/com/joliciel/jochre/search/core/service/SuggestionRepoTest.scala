package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.search.core.DocReference
import zio.Scope
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import java.time.Instant

object SuggestionRepoTest extends JUnitRunnableSpec with DatabaseTestBase {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("SuggestionRepoTest")(
    test("insert suggestion") {
      val startTime = Instant.now()
      val joe = "joe"
      val jim = "jim"
      val docRef = DocReference("doc1")
      val rectangle1 = Rectangle(10, 12, 100, 20)
      val rectangle2 = Rectangle(10, 30, 70, 20)
      val docRef2 = DocReference("doc2")
      for {
        suggestionRepo <- getSuggestionRepo()
        suggestionId1 <- suggestionRepo.insertSuggestion(joe, docRef, 42, rectangle1, "Hello", "Hi")
        suggestionId2 <- suggestionRepo.insertSuggestion(jim, docRef, 43, rectangle2, "World", "Universe")
        _ <- suggestionRepo.insertSuggestion(joe, docRef2, 44, rectangle1, "Bye", "Goodbye")
        suggestion1 <- suggestionRepo.getSuggestion(suggestionId1)
        suggestions <- suggestionRepo.getSuggestions(docRef)
        _ <- suggestionRepo.ignoreSuggestions(joe)
        suggestionsAfterIgnore <- suggestionRepo.getSuggestions(docRef)
      } yield {
        assertTrue(suggestion1.username == joe) &&
        assertTrue(suggestion1.pageIndex == 42) &&
        assertTrue(suggestion1.left == 10) &&
        assertTrue(suggestion1.top == 12) &&
        assertTrue(suggestion1.width == 100) &&
        assertTrue(suggestion1.height == 20) &&
        assertTrue(suggestion1.suggestion == "Hello") &&
        assertTrue(suggestion1.previousText == "Hi") &&
        assertTrue(suggestion1.created.toEpochMilli > startTime.toEpochMilli) &&
        assertTrue(suggestion1.ignore == false) &&
        assertTrue(suggestions.map(_.id) == Seq(suggestionId2, suggestionId1)) &&
        assertTrue(suggestionsAfterIgnore.map(_.id) == Seq(suggestionId2))
      }
    }
  ).provideLayer(suggestionRepoLayer) @@ TestAspect.sequential
}
