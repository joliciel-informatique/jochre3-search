package com.joliciel.jochre.search.core.search

import com.joliciel.jochre.search.core.{BadRequestException, NotFoundException}

class BadPdfFileFormat(message: String) extends BadRequestException(message)
class BadAltoFileFormat(message: String) extends BadRequestException(message)
class BadMetadataFileFormat(message: String) extends BadRequestException(message)
class DocumentNotFoundInIndex(message: String) extends NotFoundException(message)
class BadOffsetForImageSnippet(message: String) extends BadRequestException(message)
