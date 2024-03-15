package com.joliciel.jochre.search.api.authentication.aes

import com.joliciel.jochre.search.api.authentication.AuthenticationProvider
import com.safetydata.cloakroom.scala._
import io.circe.parser.decode

import java.util.Base64
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class AESAuthenticationProvider(password: String) extends AuthenticationProvider {
  override val tokenVerifier: TokenVerifier = AESTokenVerifier(password)
}

object AESAuthenticationProvider {
  def create(password: String): AuthenticationProvider = new AESAuthenticationProvider(password)
}

private case class AESTokenVerifier(password: String) extends TokenVerifier with JsonAuthProtocol {
  private val crypter = AESCrypter(password)
  private val b64Decoder = Base64.getDecoder
  override def apply(
      token: String
  )(implicit executionContext: ExecutionContext): Future[Either[VerificationError, VerifiedToken]] = {
    Future.successful(
      Try(crypter.decrypt(b64Decoder.decode(token))).toEither.left
        .map[VerificationError] { t => InvalidToken(Some(t), "Could not decrypt token") }
        .flatMap(str => {
          decode[RawJsonToken](str).left
            .map[VerificationError] { reason => CouldNotParseToken(None, f"Could not parse token: ${reason}") }
            .map(_.jsonToken)
        })
        .flatMap(_.toVerifiedToken)
    )
  }
}
