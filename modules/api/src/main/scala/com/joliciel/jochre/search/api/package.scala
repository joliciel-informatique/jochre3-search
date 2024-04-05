package com.joliciel.jochre.search

import com.joliciel.jochre.search.api.authentication.RoleName
import sttp.model.MediaType
import sttp.tapir.CodecFormat

package object api {
  object Roles {
    val index = RoleName("index")
  }

  val PngCodecFormat: CodecFormat = new CodecFormat {
    override def mediaType: MediaType = MediaType.ImagePng
  }

  case class OkResponse(result: String = "OK")
}
