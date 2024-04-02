package com.joliciel.jochre.search

import com.typesafe.config.ConfigFactory

import java.nio.file.Path

package object core {
  private val config = ConfigFactory.load().getConfig("jochre.search.index")
  private val contentDir = Path.of(config.getString("content-directory"))

  case class DocReference(ref: String) {
    def getBookDir(): Path = {
      contentDir.resolve(ref)
    }

    def getPageImagePath(pageNumber: Int): Path = {
      val bookDir = this.getBookDir()
      val imageFileName = f"${ref}_$pageNumber%04d.png"
      bookDir.resolve(imageFileName)
    }

    def getAltoPath(): Path = {
      val bookDir = this.getBookDir()
      val altoFileName = f"${ref}_alto4.zip"
      bookDir.resolve(altoFileName)
    }
  }

  case class DocMetadata(
      title: String,
      author: Option[String] = None,
      titleEnglish: Option[String] = None,
      authorEnglish: Option[String] = None,
      date: Option[String] = None,
      publisher: Option[String] = None,
      volume: Option[String] = None,
      url: Option[String] = None
  )
}
