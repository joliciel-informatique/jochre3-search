package com.joliciel.jochre.search.api.authentication

import com.joliciel.jochre.search.api.HttpError.Unauthorized
import com.joliciel.jochre.search.api.MainApp.getClass
import com.safetydata.cloakroom.scala.{VerificationError, VerifiedToken}
import io.circe.generic.auto._
import org.slf4j.LoggerFactory
import sttp.model.StatusCode
import sttp.tapir.PublicEndpoint
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._
import sttp.tapir.ztapir.{endpoint => tapirEndpoint, _}
import zio._

import scala.concurrent.{ExecutionContext, Future}

trait TokenAuthentication {
  private val log = LoggerFactory.getLogger(getClass)

  def authenticationProvider: AuthenticationProvider

  def secureEndpoint[R](
      requiresRoles: RoleName*
  )(implicit ec: ExecutionContext): ZPartialServerEndpoint[R, String, ValidToken, Unit, Unauthorized, Unit, Any] =
    RichZEndpoint(
      tapirEndpoint
        .securityIn(authenticationProvider.tokenEndpointInput)
        .errorOut(
          oneOf[Unauthorized](
            oneOfVariant[Unauthorized](
              StatusCode.Unauthorized,
              jsonBody[Unauthorized].description("Invalid token or missing permissions")
            )
          )
        )
    )
      .zServerSecurityLogic[R, ValidToken](principal =>
        authorize(principal, requiresRoles.toSet, authenticationProvider)
      )

  val insecureEndpoint: PublicEndpoint[Unit, Unit, Unit, Any] =
    tapirEndpoint

  private def authorize[T](
      authenticationHeaderValue: String,
      requiresRoles: Set[RoleName],
      authenticationProvider: AuthenticationProvider
  )(implicit executionContext: ExecutionContext): ZIO[Any, Unauthorized, ValidToken] =
    getTokenOrError(authenticationHeaderValue, authenticationProvider)
      .map(verifiedToken => ValidToken.fromVerifiedToken(verifiedToken, requiresRoles))
      .flatMap(ZIO.fromEither(_)) // Combine valid token error with previous errors

  private def getTokenOrError(headerValue: String, authenticationProvider: AuthenticationProvider)(implicit
      executionContext: ExecutionContext
  ): ZIO[Any, Unauthorized, VerifiedToken] = ZIO
    .fromFuture {
      val verifiedToken: Future[Either[VerificationError, VerifiedToken]] =
        authenticationProvider.tokenVerifier(extractToken(headerValue))
      _ => verifiedToken
    }
    .flatMap(ZIO.fromEither[VerificationError, VerifiedToken](_))
    .mapError {
      case error: VerificationError =>
        log.error(f"Token verification error: ${error.reason}")
        Unauthorized(f"Token verification error: ${error.reason}")
      case throwable: Throwable =>
        log.error(f"Unable to authorize", throwable)
        Unauthorized(f"Unable to authorize: ${throwable.getMessage}")
    }

  private def extractToken(headerValue: String): String = headerValue.replaceFirst("\\s_Bearer\\s+", "")
}
