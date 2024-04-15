package com.joliciel.jochre.search.api

import com.joliciel.jochre.search.core.BadRequestException

class UnknownSortException(message: String) extends BadRequestException(message)
