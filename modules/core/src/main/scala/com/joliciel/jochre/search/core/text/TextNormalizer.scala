package com.joliciel.jochre.search.core.text

import com.typesafe.config.ConfigFactory

import java.util.Locale
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

case class TextNormalizer(locale: Locale) {
  private val config = ConfigFactory.load().getConfig(f"jochre.search")
  private val configKey = f"${locale.getLanguage}.normalizer"

  private case class RegexAndReplacement(regex: Regex, replacement: String)

  private val regexList = Option
    .when(config.hasPath(configKey))(config.getConfigList(configKey).asScala.map { config =>
      val regex = config.getString("find").r
      val replacement = config.getString("replace")
      RegexAndReplacement(regex, replacement)
    })
    .getOrElse(Seq.empty)

  def normalize(string: String): String = {
    regexList.foldLeft(string) { case (string, RegexAndReplacement(regex, replacement)) =>
      regex.replaceAllIn(string, replacement)
    }
  }
}
