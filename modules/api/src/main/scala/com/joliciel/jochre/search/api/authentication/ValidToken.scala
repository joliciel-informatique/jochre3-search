package com.joliciel.jochre.search.api.authentication

import com.joliciel.jochre.search.api.HttpError.Unauthorized
import com.safetydata.cloakroom.scala.VerifiedToken

/** Corresponds to a verified token with a username, email and roles
  */
case class ValidToken private[api] (username: String, email: String, roles: Set[RoleName])

object ValidToken {

  def fromVerifiedToken(
      verifiedToken: VerifiedToken,
      requiresRoles: Set[RoleName] = Set()
  ): Either[Unauthorized, ValidToken] = {
    verifiedToken match {
      case token if token.username.isEmpty =>
        Left(Unauthorized("Token without username"))
      case token if token.email.isEmpty =>
        Left(Unauthorized("Token without email"))
      case token if !requiresRoles.map(_.name).subsetOf(token.roles) =>
        Left(Unauthorized("Unauthorized user"))
      case token =>
        Right(
          new ValidToken(username = token.username.get, email = token.email.get, roles = token.roles.map(RoleName(_)))
        )
    }
  }
}
