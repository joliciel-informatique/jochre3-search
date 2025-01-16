package com.joliciel.jochre.search.core.service

import doobie._
import doobie.implicits._
import doobie.postgres.circe.jsonb.implicits._
import doobie.postgres.implicits._
import doobie.util.meta.Meta

trait DoobieSupport {
  given Meta[DocRev] = Meta[Long].imap(DocRev.apply)(_.rev)
  given Meta[PageId] = Meta[Long].imap(PageId.apply)(_.id)
  given Meta[RowId] = Meta[Long].imap(RowId.apply)(_.id)
  given Meta[WordId] = Meta[Long].imap(WordId.apply)(_.id)
  given Meta[QueryId] = Meta[Long].imap(QueryId.apply)(_.id)
  given Meta[WordSuggestionId] = Meta[Long].imap(WordSuggestionId.apply)(_.id)
  given Meta[WordSuggestionRev] = Meta[Long].imap(WordSuggestionRev.apply)(_.rev)
  given Meta[MetadataCorrectionId] = Meta[Long].imap(MetadataCorrectionId.apply)(_.id)
  given Meta[MetadataCorrectionRev] = Meta[Long].imap(MetadataCorrectionRev.apply)(_.rev)

}
