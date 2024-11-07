package com.joliciel.jochre.search.core

import com.joliciel.jochre.search.core.service.MetadataCorrectionId

class NotFoundException(message: String) extends Exception(message)
class BadRequestException(message: String) extends Exception(message)
class ConflictException(message: String) extends Exception(message)

class DocumentAlreadyInIndexException(docRef: DocReference)
    extends ConflictException(
      f"The document ${docRef.ref} is already in the index. Delete it first or update the Alto layer only."
    )
class DocumentNotFoundInIndexException(docRef: DocReference)
    extends NotFoundException(
      f"Document ${docRef.ref} not found in index"
    )

class UnknownIndexFieldException(field: String) extends BadRequestException(f"Unknown IndexField: $field")
class WrongFieldTypeException(message: String) extends BadRequestException(message)
class NoSearchCriteriaException extends BadRequestException("No search criteria")
class PreferenceNotFound(username: String, key: String)
    extends NotFoundException(f"Preference not found for user $username, key $key")
class UnknownMetadataFieldException(field: String) extends BadRequestException(f"Unknown MetadataField: $field")
class UnknownMetadataCorrectionIdException(id: MetadataCorrectionId)
    extends NotFoundException(f"Unknown metadata correction: $id")

class PageNotFoundException(docRef: DocReference, pageNumber: Int)
    extends Exception(f"In document ${docRef.ref}, page $pageNumber not found in database")
class RowNotFoundException(docRef: DocReference, pageNumber: Int, rowIndex: Int)
    extends Exception(f"In document ${docRef.ref}, page $pageNumber, row $rowIndex not found in database")
class ImageFileNotFoundException(docRef: DocReference, pageNumber: Int)
    extends Exception(f"No image found for document ${docRef.ref}, page $pageNumber")

class UnparsableQueryException(message: String) extends BadRequestException(f"Unparsable query: $message")
