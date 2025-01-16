package com.joliciel.jochre.search.core.service

import io.circe.literal._
import zio.Scope
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

object PreferenceRepoTest extends JUnitRunnableSpec with DatabaseTestBase {
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("PreferenceRepoTest")(
    test("insert/update/get/delete preference") {
      val username = "joe"
      val key1 = "test"
      val preference1 = json"""{"langauge": "yi"}"""
      val key2 = "main"
      val preference2 = json"""{"color": "blue"}"""
      val preference2b = json"""{"color": "pink"}"""
      for {
        preferenceRepo <- getPreferenceRepo()
        _ <- preferenceRepo.upsertPreference(username, key1, preference1)
        _ <- preferenceRepo.upsertPreference(username, key2, preference2)
        retrieved1 <- preferenceRepo.getPreference(username, key1)
        retrieved2 <- preferenceRepo.getPreference(username, key2)
        _ <- preferenceRepo.upsertPreference(username, key2, preference2b)
        retrieved2b <- preferenceRepo.getPreference(username, key2)
        _ <- preferenceRepo.deletePreference(username, key2)
        deleted2 <- preferenceRepo.getPreference(username, key2)
        undeleted1 <- preferenceRepo.getPreference(username, key1)
      } yield {
        assertTrue(retrieved1 == Some(preference1)) &&
        assertTrue(retrieved2 == Some(preference2)) &&
        assertTrue(retrieved2b == Some(preference2b)) &&
        assertTrue(deleted2.isEmpty) &&
        assertTrue(undeleted1 == Some(preference1))
      }
    }
  ).provideLayer(preferenceRepoLayer) @@ TestAspect.sequential
}
