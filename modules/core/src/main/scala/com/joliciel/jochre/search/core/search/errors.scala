package com.joliciel.jochre.search.core.search

import com.joliciel.jochre.search.core.BadRequestException

class BadPdfFileFormat(message: String) extends BadRequestException(message)
class BadAltoFileFormat(message: String) extends BadRequestException(message)
class BadMetadataFileFormat(message: String) extends BadRequestException(message)
