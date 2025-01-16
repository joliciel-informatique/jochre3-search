package com.joliciel.jochre.search.api.authentication.aes

import java.time.ZonedDateTime

import io.circe._
import io.circe.generic.semiauto._

trait JsonAuthProtocol {
  case class RawJsonToken(
      username: String,
      roles: Option[Set[String]],
      email: Option[String],
      expireAt: Option[ZonedDateTime]
  ) {
    def jsonToken = JsonToken(
      username = username,
      roles = roles.getOrElse(Set()),
      email = email,
      expireAt = expireAt
    )
  }

  object RawJsonToken {
    def fromJsonToken(token: JsonToken): RawJsonToken = RawJsonToken(
      username = token.username,
      roles = Some(token.roles),
      email = token.email,
      expireAt = token.expireAt
    )
  }

  given Decoder[RawJsonToken] = deriveDecoder
  given Encoder[RawJsonToken] = deriveEncoder
}

object JsonAuthProtocol extends JsonAuthProtocol
