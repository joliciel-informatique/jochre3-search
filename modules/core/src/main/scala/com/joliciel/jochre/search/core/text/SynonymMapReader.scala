package com.joliciel.jochre.search.core.text

import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.synonym.{SolrSynonymParser, SynonymMap}
import org.slf4j.LoggerFactory

import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import java.util.Locale
import java.util.zip.ZipInputStream
import scala.jdk.CollectionConverters._

case class SynonymMapReader(locale: Locale, analyzer: Analyzer) {
  val log = LoggerFactory.getLogger(getClass)

  private val config = ConfigFactory.load().getConfig(f"jochre.search")
  private val synonymFilePath = f"${locale.getLanguage}.synonym-files"
  private val synonymFileList = if (config.hasPath(synonymFilePath)) {
    config.getStringList(synonymFilePath).asScala
  } else {
    Seq.empty
  }

  val synonymMap: Option[SynonymMap] = Option.when(synonymFileList.nonEmpty) {
    val solrSynonymParser = new SolrSynonymParser(true, true, analyzer)
    synonymFileList.foreach { synonymFilePath =>
      val inputStream = if (synonymFilePath.endsWith(".zip")) {
        val inputStream = getClass.getResourceAsStream(synonymFilePath)
        val zipInputStream = new ZipInputStream(inputStream)
        zipInputStream.getNextEntry
        zipInputStream
      } else {
        val inputStream = getClass.getResourceAsStream(synonymFilePath)
        inputStream
      }
      try {
        val reader = new InputStreamReader(inputStream, StandardCharsets.UTF_8)
        log.info(f"About to read synonyms from ${synonymFilePath}")
        solrSynonymParser.parse(reader)
        log.info(f"Synonyms read from ${synonymFilePath}")
      } finally {
        inputStream.close()
      }
    }
    solrSynonymParser.build()
  }
}

object SynonymMapReader {
  private var localeToSynonymMap: Map[Locale, Option[SynonymMap]] = Map.empty
  def getSynonymMap(locale: Locale, analyzer: Analyzer): Option[SynonymMap] = {
    if (!localeToSynonymMap.contains(locale)) {
      val synonymMapReader = SynonymMapReader(locale, analyzer)
      val synonymMap = synonymMapReader.synonymMap
      localeToSynonymMap = localeToSynonymMap + (locale -> synonymMap)
    }
    localeToSynonymMap.getOrElse(locale, None)
  }
}
