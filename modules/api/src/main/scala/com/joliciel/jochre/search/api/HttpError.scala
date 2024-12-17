package com.joliciel.jochre.search.api

sealed trait HttpError

object HttpError {
  case class InternalServerError(message: String, cause: Option[Throwable]) extends HttpError
  case class BadRequest(code: String, message: String) extends HttpError
  case class NotFound(code: String, message: String) extends HttpError
  case class Conflict(code: String, message: String) extends HttpError
  case class Unauthorized(message: String) extends HttpError
}
