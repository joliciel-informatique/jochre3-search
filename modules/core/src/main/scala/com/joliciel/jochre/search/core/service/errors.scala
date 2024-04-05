package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.{BadRequestException, NotFoundException}

class BadPdfFileFormat(message: String) extends BadRequestException(message)
class BadAltoFileFormat(message: String) extends BadRequestException(message)
class BadMetadataFileFormat(message: String) extends BadRequestException(message)
class DocumentNotFoundInIndex(message: String) extends NotFoundException(message)
class BadOffsetForImageSnippet(message: String) extends BadRequestException(message)
class IndexFieldNotAggregatable(message: String) extends BadRequestException(message)
class InvalidJsonException(message: String) extends BadRequestException(message)
