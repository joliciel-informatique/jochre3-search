package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.api.HttpError.{BadRequest, Conflict, InternalServerError, NotFound}
import com.joliciel.jochre.search.core.{BadRequestException, ConflictException, NotFoundException}

trait HttpErrorMapper {
  def mapToHttpError(exception: Throwable): HttpError = exception match {
    case e: NotFoundException   => NotFound(e.code, e.getMessage)
    case e: BadRequestException => BadRequest(e.code, e.getMessage)
    case e: ConflictException   => Conflict(e.code, e.getMessage)
    case error: Throwable       => InternalServerError(message = error.getMessage, cause = Some(error))
  }
}
