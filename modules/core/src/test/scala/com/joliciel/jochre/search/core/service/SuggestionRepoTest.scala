package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.search.core.{DocReference, MetadataField}
import zio.Scope
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import java.time.Instant

object SuggestionRepoTest extends JUnitRunnableSpec with DatabaseTestBase {
  override def spec: Spec[TestEnvironment with Scope, Any] = suite("SuggestionRepoTest")(
    test("insert suggestion") {
      val startTime = Instant.now()
      val joe = "joe"
      val joeIp = Some("127.0.0.1")
      val jim = "jim"
      val jimIp = None
      val docRef = DocReference("doc1")
      val rectangle1 = Rectangle(10, 12, 100, 20)
      val rectangle2 = Rectangle(10, 30, 70, 20)
      val docRef2 = DocReference("doc2")
      for {
        suggestionRepo <- getSuggestionRepo()
        suggestionId1 <- suggestionRepo.insertSuggestion(joe, joeIp, docRef, 42, rectangle1, "Hello", "Hi", 10)
        suggestionId2 <- suggestionRepo.insertSuggestion(jim, jimIp, docRef, 43, rectangle2, "World", "Universe", 20)
        _ <- suggestionRepo.insertSuggestion(joe, joeIp, docRef2, 44, rectangle1, "Bye", "Goodbye", 30)
        suggestion1 <- suggestionRepo.getSuggestion(suggestionId1)
        suggestions <- suggestionRepo.getSuggestions(docRef)
        _ <- suggestionRepo.ignoreSuggestions(joe)
        suggestionsAfterIgnore <- suggestionRepo.getSuggestions(docRef)
        suggestion1AfterIgnore <- suggestionRepo.getSuggestion(suggestionId1)
      } yield {
        assertTrue(suggestion1.username == joe) &&
        assertTrue(suggestion1.ipAddress == joeIp) &&
        assertTrue(suggestion1.pageIndex == 42) &&
        assertTrue(suggestion1.left == 10) &&
        assertTrue(suggestion1.top == 12) &&
        assertTrue(suggestion1.width == 100) &&
        assertTrue(suggestion1.height == 20) &&
        assertTrue(suggestion1.suggestion == "Hello") &&
        assertTrue(suggestion1.previousText == "Hi") &&
        assertTrue(suggestion1.created.toEpochMilli > startTime.toEpochMilli) &&
        assertTrue(!suggestion1.ignore) &&
        assertTrue(suggestion1.offset == 10) &&
        assertTrue(suggestion1.rev.rev > 0) &&
        assertTrue(suggestions.map(_.id) == Seq(suggestionId2, suggestionId1)) &&
        // The suggestions should be re-ordered with the ignored suggestion on top
        assertTrue(suggestionsAfterIgnore.map(_.id) == Seq(suggestionId1, suggestionId2)) &&
        assertTrue(suggestion1AfterIgnore.rev.rev > suggestion1.rev.rev) &&
        assertTrue(suggestion1AfterIgnore.ignore)
      }
    },
    test("insert metadata correction") {
      val startTime = Instant.now()
      val joe = "joe"
      val joeIp = Some("127.0.0.1")
      val jim = "jim"
      val jimIp = None
      val docRef = DocReference("doc1")
      val docRef2 = DocReference("doc2")
      for {
        suggestionRepo <- getSuggestionRepo()
        correctionId1 <- suggestionRepo.insertMetadataCorrection(
          joe,
          joeIp,
          MetadataField.Author,
          Some("Joe Schmoe"),
          "Joseph Schmozeph",
          applyEverywhere = true,
          Seq(docRef, docRef2)
        )
        correctionId2 <- suggestionRepo.insertMetadataCorrection(
          jim,
          jimIp,
          MetadataField.Publisher,
          None,
          "Schmoe Editions Ltd",
          applyEverywhere = false,
          Seq(docRef)
        )
        correction1 <- suggestionRepo.getMetadataCorrection(correctionId1)
        docs <- suggestionRepo.getMetadataCorrectionDocs(correctionId1)
        corrections <- suggestionRepo.getMetadataCorrections(docRef)
        correctionId3 <- suggestionRepo.insertMetadataCorrection(
          joe,
          joeIp,
          MetadataField.Author,
          Some("Joseph Schmozeph"),
          "Joe Schmoe",
          applyEverywhere = false,
          Seq(docRef)
        )
        correctionsAfterUpdate <- suggestionRepo.getMetadataCorrections(docRef)
        _ <- suggestionRepo.ignoreMetadataCorrections(joe)
        correctionsAfterIgnore <- suggestionRepo.getMetadataCorrections(docRef)
        _ <- suggestionRepo.ignoreMetadataCorrection(correctionId2)
        correctionsAfterIgnoreMore <- suggestionRepo.getMetadataCorrections(docRef)
      } yield {
        assertTrue(correction1.username == joe) &&
        assertTrue(correction1.ipAddress == joeIp) &&
        assertTrue(correction1.field == MetadataField.Author) &&
        assertTrue(correction1.oldValue.contains("Joe Schmoe")) &&
        assertTrue(correction1.newValue == "Joseph Schmozeph") &&
        assertTrue(correction1.created.toEpochMilli > startTime.toEpochMilli) &&
        assertTrue(correction1.applyEverywhere) &&
        assertTrue(!correction1.ignore) &&
        assertTrue(!correction1.sent) &&
        assertTrue(correction1.rev.rev > 0) &&
        assertTrue(docs == Seq(docRef, docRef2)) &&
        assertTrue(corrections.map(_.id) == Seq(correctionId2, correctionId1)) &&
        assertTrue(correctionsAfterUpdate.map(_.id) == Seq(correctionId3, correctionId2, correctionId1)) &&
        assertTrue(correctionsAfterIgnore.map(_.id) == Seq(correctionId3, correctionId1, correctionId2)) &&
        assertTrue(correctionsAfterIgnoreMore.map(_.id) == Seq(correctionId2, correctionId3, correctionId1))
      }
    }
  ).provideLayer(suggestionRepoLayer) @@ TestAspect.sequential
}
