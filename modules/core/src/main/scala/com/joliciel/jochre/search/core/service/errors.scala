package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.{BadRequestException, DocReference, NotFoundException}

class BadPdfFileFormat(message: String) extends BadRequestException(message)
class BadAltoFileFormat(message: String) extends BadRequestException(message)
class BadImageZipFileFormat(message: String) extends BadRequestException(message)
class BadMetadataFileFormat(message: String) extends BadRequestException(message)
class BadOffsetForImageSnippet(message: String) extends BadRequestException(message)
class IndexFieldNotAggregatable(message: String) extends BadRequestException(message)
class NoFieldRequestedForAggregation extends BadRequestException("No fields were selected for aggregation")
class InvalidJsonException(message: String) extends BadRequestException(message)
class WordOffsetNotFound(docRef: DocReference, wordOffset: Int)
    extends BadRequestException(f"No word found for document ${docRef.ref}, offset $wordOffset")
