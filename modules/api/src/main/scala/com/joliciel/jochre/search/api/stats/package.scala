package com.joliciel.jochre.search.api

import enumeratum.{Enum, EnumEntry}

package object stats {
  case class UsageStatsBin(
      label: String,
      distinctUsers: Int,
      queries: Int
  )

  case class UsageStats(
      bins: Seq[UsageStatsBin]
  )

  object StatsHelper {
    val usageStatsExample: UsageStats = UsageStats(
      Seq(
        UsageStatsBin("2025-01-14", 41, 735),
        UsageStatsBin("2025-01-15", 63, 938)
      )
    )
  }

  enum TimeUnit:
    case Day, Month, Year
}
