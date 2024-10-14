package com.joliciel.jochre.search.api.users

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.ValidToken
import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper, OkResponse}
import com.joliciel.jochre.search.core.PreferenceNotFound
import com.joliciel.jochre.search.core.service.PreferenceService
import io.circe.Json
import zio.ZIO

trait UserLogic extends HttpErrorMapper {
  def upsertPreferenceLogic(
      token: ValidToken,
      key: String,
      preference: Json
  ): ZIO[Requirements, HttpError, OkResponse] = {
    for {
      preferenceService <- ZIO.service[PreferenceService]
      _ <- preferenceService.upsertPreference(token.username, key, preference)
    } yield OkResponse()
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to upsert preferences", error))
    .mapError(mapToHttpError)

  def getPreferenceLogic(token: ValidToken, key: String): ZIO[Requirements, HttpError, Json] = {
    for {
      preferenceService <- ZIO.service[PreferenceService]
      json <- preferenceService.getPreference(token.username, key)
    } yield json
  }.foldZIO(
    error => ZIO.fail(error),
    success =>
      success match {
        case Some(json) => ZIO.succeed(json)
        case None       => ZIO.fail(new PreferenceNotFound(token.username, key))
      }
  ).tapErrorCause(error => ZIO.logErrorCause(s"Unable to get preferences", error))
    .mapError(mapToHttpError)

  def deletePreferenceLogic(token: ValidToken, key: String): ZIO[Requirements, HttpError, OkResponse] = {
    for {
      preferenceService <- ZIO.service[PreferenceService]
      _ <- preferenceService.deletePreference(token.username, key)
    } yield OkResponse()
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to delete preferences", error))
    .mapError(mapToHttpError)
}
