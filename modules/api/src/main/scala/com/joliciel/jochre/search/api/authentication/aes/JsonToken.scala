package com.joliciel.jochre.search.api.authentication.aes

import java.time.ZonedDateTime

import com.safetydata.cloakroom.scala.{InvalidToken, VerificationError, VerifiedToken}

case class JsonToken(
    username: String,
    roles: Set[String] = Set(),
    email: Option[String] = None,
    expireAt: Option[ZonedDateTime] = None
) {
  def toVerifiedToken: Either[VerificationError, VerifiedToken] = {
    Either.cond(
      expireAt.map(_.isAfter(ZonedDateTime.now())).getOrElse(true),
      VerifiedToken.fake(
        username = Some(username),
        email = email,
        roles = roles,
        forClient = "jochre"
      ),
      InvalidToken(None, "token expired")
    )
  }
}
