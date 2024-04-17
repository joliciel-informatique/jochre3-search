package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.search.core.DocReference
import doobie.Transactor
import doobie.implicits._
import doobie.postgres.implicits._
import zio.interop.catz._
import zio.{Task, ZIO, ZLayer}

private[service] case class SuggestionRepo(transactor: Transactor[Task]) {
  def insertSuggestion(
      username: String,
      docRef: DocReference,
      pageIndex: Int,
      rectangle: Rectangle,
      suggestion: String,
      previousText: String
  ): Task[WordSuggestionId] =
    sql"""INSERT INTO word_suggestion (username, doc_ref, page_index, lft, top, width, height, suggestion, previous_text)
         | VALUES ($username, ${docRef.ref}, $pageIndex, ${rectangle.left}, ${rectangle.top}, ${rectangle.width}, ${rectangle.height}, $suggestion, $previousText)
         | RETURNING id
       """.stripMargin
      .query[WordSuggestionId]
      .unique
      .transact(transactor)

  def getSuggestion(id: WordSuggestionId): Task[DbWordSuggestion] =
    sql"""SELECT id, username, created, doc_ref, page_index, lft, top, width, height, suggestion, previous_text, ignore
         | FROM word_suggestion
         | WHERE id = ${id.id}
       """.stripMargin
      .query[DbWordSuggestion]
      .unique
      .transact(transactor)

  /** Get suggestions for a given document from newest to oldest.
    */
  def getSuggestions(docRef: DocReference): Task[Seq[DbWordSuggestion]] = {
    sql"""SELECT id, username, created, doc_ref, page_index, lft, top, width, height, suggestion, previous_text, ignore
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

  private val deleteAllSuggestions: Task[Int] =
    sql"""DELETE FROM word_suggestion""".update.run.transact(transactor)

  private[service] val deleteAll: Task[Int] =
    for {
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
