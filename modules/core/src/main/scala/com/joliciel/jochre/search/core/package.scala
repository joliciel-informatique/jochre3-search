package com.joliciel.jochre.search

package object core {

  case class DocReference(ref: String)

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
