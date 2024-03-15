package com.joliciel.jochre.search.core.lucene

import org.apache.lucene.document.{Field, FieldType}
import org.apache.lucene.index.IndexOptions

private[lucene] class StoredTextField(name: String, contents: String) extends Field(name, StoredTextField.Type) {
  fieldsData = contents
}

object StoredTextField {
  val Type = {
    val fieldType = new FieldType()
    fieldType.setTokenized(true)
    fieldType.setStored(true)
    fieldType.setStoreTermVectors(true)
    fieldType.setStoreTermVectorOffsets(true)
    fieldType.setStoreTermVectorPositions(true)
    fieldType.setStoreTermVectorPayloads(true)
    fieldType.setIndexOptions(IndexOptions.DOCS_AND_FREQS_AND_POSITIONS_AND_OFFSETS)
    fieldType.freeze()
    fieldType
  }
}