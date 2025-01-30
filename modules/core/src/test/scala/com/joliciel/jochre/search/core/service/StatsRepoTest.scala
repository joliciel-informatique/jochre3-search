package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.search.core.{DocReference, MetadataField}
import zio.Scope
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import java.time.Instant
import com.joliciel.jochre.search.core.TimeUnit
import com.joliciel.jochre.search.core.UsageStats
import com.joliciel.jochre.search.core.UsageStatsBin
import java.time.format.DateTimeFormatter
import com.joliciel.jochre.search.core.SearchCriterion
import com.joliciel.jochre.search.core.Sort
import com.joliciel.jochre.search.core.IndexField
import java.time.ZoneOffset
import java.time.temporal.ChronoUnit
import java.time.ZoneId

object StatsRepoTest extends JUnitRunnableSpec with DatabaseTestBase {
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("StatsRepoTest")(
    test("get usage stats") {
      val dayFormatter = DateTimeFormatter
        .ofPattern("yyyy-MM-dd")
        .withZone(ZoneId.systemDefault())
      val startTime = Instant.now()
      val joe = "Joe"
      val joeIp = Some("127.0.0.1")
      val jim = "Jim"
      val criteria = SearchCriterion.And(
        SearchCriterion.Contains(IndexField.Text, "Hello"),
        SearchCriterion.Not(SearchCriterion.ValueIn(IndexField.Author, Seq("Joe Schmoe")))
      )
      val sort = Sort.Field(IndexField.PublicationYearAsNumber, ascending = true)
      for {
        statsRepo <- getStatsRepo()
        searchRepo <- getSearchRepo()
        _ <- searchRepo.insertQuery(joe, joeIp, criteria, sort, 20, 42, 72)
        _ <- searchRepo.insertQuery(joe, joeIp, criteria, sort, 30, 42, 72)
        _ <- searchRepo.insertQuery(jim, joeIp, criteria, sort, 20, 42, 72)
        usageStats <- statsRepo.getUsageStats(
          TimeUnit.Day,
          startTime,
          startTime
        ) // Note endTime will get one day added inside.
      } yield {
        assertTrue(
          usageStats == UsageStats(
            Seq(
              UsageStatsBin(dayFormatter.format(startTime), 2, 3)
            )
          )
        )
      }
    }
  ).provideLayer(searchRepoLayer ++ statsRepoLayer) @@ TestAspect.sequential
}
