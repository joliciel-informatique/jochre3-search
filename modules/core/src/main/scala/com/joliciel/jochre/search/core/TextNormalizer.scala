package com.joliciel.jochre.search.core

import com.typesafe.config.ConfigFactory

import java.util.Locale
import scala.util.matching.Regex
import scala.jdk.CollectionConverters._

case class TextNormalizer(locale: Locale) {
  private val config = ConfigFactory.load().getConfig(f"jochre.search.normalizer")
  private val configList = config.getConfigList(locale.getLanguage)

  case class RegexAndReplacement(regex: Regex, replacement: String)

  private val regexList = configList.asScala.map{ config =>
    val regex = config.getString("find").r
    val replacement = config.getString("replace")
    RegexAndReplacement(regex, replacement)
  }

  def normalize(string: String): String = {
    regexList.foldLeft(string){ case (string, RegexAndReplacement(regex, replacement)) =>
      regex.replaceAllIn(string, replacement)
    }
  }
}
