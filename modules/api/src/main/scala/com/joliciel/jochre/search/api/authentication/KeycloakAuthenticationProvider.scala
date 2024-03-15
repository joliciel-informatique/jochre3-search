package com.joliciel.jochre.search.api.authentication

import com.safetydata.cloakroom.CloakroomConfig
import com.safetydata.cloakroom.scala.TokenVerifier
import sttp.tapir.{EndpointInput, auth}
import zio._

import scala.collection.immutable.ListMap

case class KeycloakAuthenticationProvider(
    tokenVerifier: TokenVerifier,
    override val tokenEndpointInput: EndpointInput[String]
) extends AuthenticationProvider

object KeycloakAuthenticationProvider {
  def create(realm: String, timeout: Option[Duration]): AuthenticationProvider = {
    val keycloakDeployment = CloakroomConfig.fromTypeSafeConf(realm).keycloakDeployment()

    val endpoint = auth.oauth2.authorizationCode(
      authorizationUrl = Some(keycloakDeployment.getAuthUrl.build().toString),
      tokenUrl = Some(keycloakDeployment.getTokenUrl),
      scopes = ListMap()
    )

    KeycloakAuthenticationProvider(TokenVerifier(realm), endpoint)
  }
}
