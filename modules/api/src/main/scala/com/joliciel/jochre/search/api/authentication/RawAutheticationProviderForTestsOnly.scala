package com.joliciel.jochre.search.api.authentication

import com.safetydata.cloakroom.scala.{InvalidToken, TokenVerifier, VerificationError, VerifiedToken}
import com.typesafe.config.{Config, ConfigException, ConfigFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._

object RawAuthenticationProviderForTestsOnly extends AuthenticationProvider {
  val tokenVerifier: TokenVerifier = RawTokenVerifierForTestsOnly

  val defaultToken: VerifiedToken = VerifiedToken.fake(
    username = Some(RawTokenVerifierForTestsOnly.default.username),
    email = Some(RawTokenVerifierForTestsOnly.default.email),
    roles = RawTokenVerifierForTestsOnly.default.roles.map(_.name),
    forClient = RawTokenVerifierForTestsOnly.default.client
  )

  val defaultValidToken: ValidToken = ValidToken(
    RawTokenVerifierForTestsOnly.default.username,
    RawTokenVerifierForTestsOnly.default.email,
    RawTokenVerifierForTestsOnly.default.roles
  )

  def validTokenForTestOnly(
      username: String = RawTokenVerifierForTestsOnly.default.username,
      email: String = RawTokenVerifierForTestsOnly.default.email,
      roles: Set[RoleName] = RawTokenVerifierForTestsOnly.default.roles
  ): ValidToken =
    ValidToken(username, email, roles)

  def create(): AuthenticationProvider = RawAuthenticationProviderForTestsOnly
}

private object RawTokenVerifierForTestsOnly extends TokenVerifier {

  object default {
    val username = "test"
    val email = "test@test.com"
    val roles: Set[RoleName] = Set()
    val client = "client"
  }

  override def apply(
      token: String
  )(implicit executionContext: ExecutionContext): Future[Either[VerificationError, VerifiedToken]] = {
    val defaultToken = ConfigFactory.parseMap(
      Map[String, Object](
        "username" -> default.username,
        "email" -> default.email,
        "roles" -> default.roles.toList.map(_.name).asJava,
        "fail" -> java.lang.Boolean.FALSE
      ).asJava
    )

    Future.successful(
      try {
        configToToken(
          ConfigFactory
            .parseString(token)
            .withFallback(defaultToken)
        )
      } catch {
        case e: ConfigException => Left(InvalidToken(Some(e), reason = "Could not parse raw token"))
      }
    )
  }

  private def configToToken(config: Config): Either[VerificationError, VerifiedToken] = {
    def readOptional(name: String): Option[String] = {
      if (config.getIsNull(name)) {
        None
      } else {
        Some(config.getString(name))
      }
    }
    if (config.getBoolean("fail")) {
      Left(InvalidToken(None, "Raw token is marked invalid"))
    } else {
      Right(
        VerifiedToken.fake(
          username = readOptional("username"),
          email = readOptional("email"),
          roles = config.getStringList("roles").asScala.toSet,
          forClient = default.client
        )
      )
    }
  }
}
