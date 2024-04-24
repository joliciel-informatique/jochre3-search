package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.search.core.{DocReference, MetadataField}
import doobie.Transactor
import doobie.implicits._
import doobie.postgres.implicits._
import doobie.util.update.Update
import zio.interop.catz._
import zio.{Task, ZIO, ZLayer}

private[service] case class SuggestionRepo(transactor: Transactor[Task]) {
  def insertSuggestion(
      username: String,
      ipAddress: Option[String],
      docRef: DocReference,
      pageIndex: Int,
      rectangle: Rectangle,
      suggestion: String,
      previousText: String
  ): Task[WordSuggestionId] =
    sql"""INSERT INTO word_suggestion (username, ip, doc_ref, page_index, lft, top, width, height, suggestion, previous_text)
         | VALUES ($username, $ipAddress, ${docRef.ref}, $pageIndex, ${rectangle.left}, ${rectangle.top}, ${rectangle.width}, ${rectangle.height}, $suggestion, $previousText)
         | RETURNING id
       """.stripMargin
      .query[WordSuggestionId]
      .unique
      .transact(transactor)

  def getSuggestion(id: WordSuggestionId): Task[DbWordSuggestion] =
    sql"""SELECT id, username, ip, created, doc_ref, page_index, lft, top, width, height, suggestion, previous_text, ignore
         | FROM word_suggestion
         | WHERE id = ${id.id}
       """.stripMargin
      .query[DbWordSuggestion]
      .unique
      .transact(transactor)

  /** Get suggestions for a given document from newest to oldest.
    */
  def getSuggestions(docRef: DocReference): Task[Seq[DbWordSuggestion]] = {
    sql"""SELECT id, username, ip, created, doc_ref, page_index, lft, top, width, height, suggestion, previous_text, ignore
         | FROM word_suggestion
         | WHERE doc_ref = ${docRef.ref}
         | AND ignore = ${false}
         | ORDER BY id DESC
       """.stripMargin
      .query[DbWordSuggestion]
      .to[Seq]
      .transact(transactor)
  }

  def ignoreSuggestions(username: String): Task[Int] =
    sql"""UPDATE word_suggestion
          SET ignore=${true}
          WHERE username=$username
       """.stripMargin.update.run
      .transact(transactor)

  def insertMetadataCorrection(
      username: String,
      ipAddress: Option[String],
      field: MetadataField,
      oldValue: Option[String],
      newValue: String,
      applyEverywhere: Boolean,
      docRefs: Seq[DocReference]
  ): Task[MetadataCorrectionId] = {
    (for {
      correctionId <- insertMetadataCorrection(username, ipAddress, field, oldValue, newValue, applyEverywhere)
      _ <- insertMetadataCorrectionDocuments(correctionId, docRefs)
    } yield correctionId).transact(transactor)
  }

  private def insertMetadataCorrection(
      username: String,
      ipAddress: Option[String],
      field: MetadataField,
      oldValue: Option[String],
      newValue: String,
      applyEverywhere: Boolean
  ) = {
    sql"""INSERT INTO metadata_correction (username, ip, field, old_value, new_value, apply_everywhere)
         | VALUES ($username, $ipAddress, $field, $oldValue, $newValue, $applyEverywhere)
         | RETURNING id
       """.stripMargin
      .query[MetadataCorrectionId]
      .unique
  }

  private def insertMetadataCorrectionDocuments(
      metadataCorrectionId: MetadataCorrectionId,
      docRefs: Seq[DocReference]
  ) = {
    val tuples = docRefs.map(d => metadataCorrectionId -> d)
    val sql = "INSERT INTO metadata_correction_doc (correction_id, doc_ref) VALUES (?, ?)"
    Update[(MetadataCorrectionId, DocReference)](sql).updateMany(tuples)
  }

  def getMetadataCorrection(id: MetadataCorrectionId): Task[DbMetadataCorrection] =
    sql"""SELECT id, username, ip, created, field, old_value, new_value, apply_everywhere, ignore, sent
         | FROM metadata_correction
         | WHERE id = ${id.id}
       """.stripMargin
      .query[DbMetadataCorrection]
      .unique
      .transact(transactor)

  def getMetadataCorrectionDocs(id: MetadataCorrectionId): Task[Seq[DocReference]] =
    sql"""SELECT doc_ref FROM metadata_correction_doc WHERE correction_id=$id
          | ORDER BY doc_ref""".stripMargin
      .query[DocReference]
      .to[Seq]
      .transact(transactor)

  /** Get most recent corrections for a given document.
    */
  def getMetadataCorrections(docRef: DocReference): Task[Seq[DbMetadataCorrection]] = {
    sql"""SELECT id, username, ip, created, field, old_value, new_value, apply_everywhere, ignore, sent
         | FROM metadata_correction m1
         | INNER JOIN metadata_correction_doc d1 ON m1.id = d1.correction_id
         | WHERE d1.doc_ref = ${docRef.ref}
         | AND ignore = ${false}
         | AND id = (SELECT MAX(m2.id) FROM metadata_correction m2
         |  INNER JOIN metadata_correction_doc d2 ON m2.id = d2.correction_id WHERE m2.field = m1.field AND d2.doc_ref = d1.doc_ref)
         | ORDER BY id DESC
       """.stripMargin
      .query[DbMetadataCorrection]
      .to[Seq]
      .transact(transactor)
  }

  def ignoreMetadataCorrections(username: String): Task[Int] =
    sql"""UPDATE metadata_correction
          SET ignore=${true}
          WHERE username=$username
       """.stripMargin.update.run
      .transact(transactor)

  def ignoreMetadataCorrection(id: MetadataCorrectionId): Task[Int] =
    sql"""UPDATE metadata_correction
          SET ignore=${true}
          WHERE id=$id
       """.stripMargin.update.run
      .transact(transactor)

  def markMetadataCorrectionAsSent(id: MetadataCorrectionId): Task[Int] =
    sql"""UPDATE metadata_correction
          SET sent=${true}
          WHERE id=$id
       """.stripMargin.update.run
      .transact(transactor)

  private val deleteAllSuggestions: Task[Int] =
    sql"""DELETE FROM word_suggestion""".update.run.transact(transactor)

  private val deleteAllMetadataCorrections: Task[Int] =
    sql"""DELETE FROM metadata_correction""".update.run.transact(transactor)

  private[service] val deleteAll: Task[Int] =
    for {
      _ <- deleteAllMetadataCorrections
      count <- deleteAllSuggestions
    } yield count
}

object SuggestionRepo {
  val live: ZLayer[Transactor[Task], Nothing, SuggestionRepo] =
    ZLayer {
      for {
        transactor <- ZIO.service[Transactor[Task]]
      } yield SuggestionRepo(transactor)
    }
}
