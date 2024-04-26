package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.{BadRequestException, DocReference, NotFoundException}

class BadPdfFileFormat(message: String) extends BadRequestException(message)
class BadAltoFileFormat(message: String) extends BadRequestException(message)
class BadImageZipFileFormat(message: String) extends BadRequestException(message)
class BadMetadataFileFormat(message: String) extends BadRequestException(message)
class DocumentNotFoundInIndex(docRef: DocReference) extends NotFoundException(f"Document ${docRef.ref} not found in index")
class BadOffsetForImageSnippet(message: String) extends BadRequestException(message)
class IndexFieldNotAggregatable(message: String) extends BadRequestException(message)
class InvalidJsonException(message: String) extends BadRequestException(message)
class WordOffsetNotFound(docRef: DocReference, wordOffset: Int)
    extends BadRequestException(f"No word found for document ${docRef.ref}, offset $wordOffset")
