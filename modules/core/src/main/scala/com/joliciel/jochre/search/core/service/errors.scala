package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.{BadRequestException, DocReference}

class BadPdfFileFormat(message: String) extends BadRequestException("BadPdfFileFormat", message)
class BadAltoFileFormat(message: String) extends BadRequestException("BadAltoFileFormat", message)
class BadImageZipFileFormat(message: String) extends BadRequestException("BadImageZipFileFormat", message)
class BadMetadataFileFormat(message: String) extends BadRequestException("BadMetadataFileFormat", message)
class BadOffsetForImageSnippet(message: String) extends BadRequestException("BadOffsetForImageSnippet", message)
class IndexFieldNotAggregatable(message: String) extends BadRequestException("IndexFieldNotAggregatable", message)
class NoFieldRequestedForAggregation
    extends BadRequestException("NoFieldRequestedForAggregation", "No fields were selected for aggregation")
class WordOffsetNotFound(docRef: DocReference, wordOffset: Int)
    extends BadRequestException("WordOffsetNotFound", f"No word found for document ${docRef.ref}, offset $wordOffset")
