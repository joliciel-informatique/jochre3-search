package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.{BadRequestException, NotFoundException}

class UnknownSortException(message: String) extends BadRequestException("UnknownSort", message)
class UnknownLogLevelException(logLevel: String)
    extends NotFoundException("UnknownLogLevel", f"Unknown log level: $logLevel")
