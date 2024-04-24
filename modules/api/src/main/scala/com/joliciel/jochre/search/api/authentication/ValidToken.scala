package com.joliciel.jochre.search.api.authentication

import com.joliciel.jochre.search.api.HttpError.Unauthorized
import com.safetydata.cloakroom.scala.VerifiedToken
import org.slf4j.LoggerFactory

/** Corresponds to a verified token with a username, email and roles
  */
case class ValidToken private[api] (username: String, email: String, roles: Set[RoleName])

object ValidToken {
  private val log = LoggerFactory.getLogger(getClass)

  def fromVerifiedToken(
      verifiedToken: VerifiedToken,
      requiresRoles: Set[RoleName] = Set()
  ): Either[Unauthorized, ValidToken] = {
    verifiedToken match {
      case token if token.username.isEmpty =>
        log.warn(f"User has no username")
        Left(Unauthorized("Token without username"))
      case token if token.email.isEmpty =>
        log.warn(f"User ${token.username.get} has no email")
        Left(Unauthorized("Token without email"))
      case token if !requiresRoles.map(_.name).subsetOf(token.roles) =>
        log.warn(f"User ${token.username.get} has roles ${token.roles
          .mkString(",")}. Required: ${requiresRoles.map(_.name).mkString(",")}")
        Left(Unauthorized("Unauthorized user"))
      case token =>
        Right(
          new ValidToken(username = token.username.get, email = token.email.get, roles = token.roles.map(RoleName(_)))
        )
    }
  }
}
