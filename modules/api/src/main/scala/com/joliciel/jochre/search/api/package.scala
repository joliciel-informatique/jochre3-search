package com.joliciel.jochre.search

import sttp.model.MediaType
import sttp.tapir.CodecFormat

package object api {
  val PngCodecFormat: CodecFormat = new CodecFormat {
    override def mediaType: MediaType = MediaType.ImagePng
  }
}
