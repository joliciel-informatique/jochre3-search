package com.joliciel.jochre.search.api.authentication

import com.joliciel.jochre.search.api.authentication.aes.AESAuthenticationProvider
import com.safetydata.cloakroom.scala.TokenVerifier
import sttp.tapir._
import zio.config.magnolia._
import zio.{Duration, Config => ZioConfig}

trait AuthenticationProvider {
  def tokenEndpointInput: EndpointInput[String] = auth.bearer[String]()
  def tokenVerifier: TokenVerifier
}

object AuthenticationProvider {
  sealed trait ProviderType

  object ProviderType {
    case class Keycloak(realm: String, timeout: Option[Duration]) extends ProviderType
    case class AES(password: String) extends ProviderType
    case object RawForTests extends ProviderType
  }

  def apply(config: AuthenticationProviderConfig): AuthenticationProvider = config.providerType match {
    case ProviderType.Keycloak(realm, timeout) => KeycloakAuthenticationProvider.create(realm, timeout)
    case ProviderType.AES(password)            => AESAuthenticationProvider.create(password)
    case ProviderType.RawForTests              => RawAuthenticationProviderForTestsOnly.create()
  }
}

case class AuthenticationProviderConfig(providerType: AuthenticationProvider.ProviderType)

object AuthenticationProviderConfig {
  val config: ZioConfig[AuthenticationProviderConfig] =
    deriveConfig[AuthenticationProviderConfig].nested("authentication-provider")
}
