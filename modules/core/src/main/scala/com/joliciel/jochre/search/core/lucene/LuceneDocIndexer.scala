package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.{AltoDocument, FieldKind, IndexField, WrongFieldTypeException}
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.{Document => IndexDocument, _}
import org.apache.lucene.facet.sortedset.SortedSetDocValuesFacetField
import org.apache.lucene.index.{IndexWriter, IndexableField, Term}
import org.apache.lucene.util.BytesRef
import org.slf4j.LoggerFactory

import java.time.Instant

private[search] case class LuceneDocIndexer(
    private val indexWriter: IndexWriter,
    private val indexingHelper: IndexingHelper
) {
  private val log = LoggerFactory.getLogger(getClass)

  def indexDocument(doc: AltoDocument): Unit = {
    try {
      log.info(f"Indexing ${doc.ref.ref}")
      val docTerm = new Term(IndexField.Reference.fieldName, doc.ref.ref)
      val fields = toLuceneDoc(doc)
      indexWriter.updateDocument(docTerm, fields)
      log.info(f"Indexed ${doc.ref.ref}")
      indexingHelper.removeDocumentInfo(doc.ref)
      commit()
      log.info(f"Index commited for ${doc.ref.ref}")
    } catch {
      case exception: Exception =>
        log.error(f"Failed to index ${doc.ref.ref}", exception)
        throw exception
    }
  }

  def commit(): Unit = {
    indexWriter.commit()
  }

  private def fieldsToLuceneDoc(fields: Seq[IndexableField]): IndexDocument = {
    val luceneDoc = new IndexDocument
    fields.foreach(luceneDoc.add(_))
    FacetConfigHolder.facetsConfig.build(luceneDoc)
  }

  private def toLuceneDoc(
      doc: AltoDocument
  ): IndexDocument = fieldsToLuceneDoc(
    Seq(
      getFieldsForString(IndexField.Reference, doc.ref.ref),
      Seq(new StringField(IndexField.Revision.fieldName, doc.rev.rev.toString, Store.YES)),
      doc.metadata.title.map(getFieldsForText(IndexField.Title, _)).getOrElse(Seq.empty),
      doc.metadata.titleEnglish.map(getFieldsForText(IndexField.TitleEnglish, _)).getOrElse(Seq.empty),
      getFieldsForText(IndexField.Text, doc.text),
      doc.metadata.author.map(getFieldsForUntokenizedText(IndexField.Author, _)).getOrElse(Seq.empty),
      doc.metadata.authorEnglish.map(getFieldsForUntokenizedText(IndexField.AuthorEnglish, _)).getOrElse(Seq.empty),
      doc.metadata.publisher.map(getFieldsForUntokenizedText(IndexField.Publisher, _)).getOrElse(Seq.empty),
      doc.metadata.volume.map(getFieldsForUntokenizedText(IndexField.Volume, _)).getOrElse(Seq.empty),
      doc.metadata.publicationYear
        .map(getFieldsForYear(IndexField.PublicationYear, IndexField.PublicationYearAsNumber, _))
        .getOrElse(Seq.empty),
      doc.metadata.url.map(getFieldsForString(IndexField.URL, _)).getOrElse(Seq.empty),
      getFieldsForMultiString(IndexField.Collection, doc.metadata.collections),
      doc.ocrSoftware.map(getFieldsForString(IndexField.OCRSoftware, _)).getOrElse(Seq.empty),
      getFieldsForInstant(IndexField.IndexTime, Instant.now())
    ).flatten
  )

  private def getFieldsForText(
      field: IndexField,
      text: String
  ): Seq[IndexableField] = {
    if (field.kind != FieldKind.Text) {
      throw new WrongFieldTypeException(f"Cannot create text field for field ${field.fieldName}")
    }
    Seq(
      new StoredTextField(field.fieldName, text)
    )
  }

  private def getFieldsForString(
      field: IndexField,
      value: String
  ): Seq[IndexableField] = {
    if (field.kind != FieldKind.String) {
      throw new WrongFieldTypeException(f"Cannot create string field for field ${field.fieldName}")
    }
    val stringField = new StringField(field.fieldName, value, Store.YES)
    val sortField = Option.when(field.sortable && value != null && !value.isBlank)(
      new SortedDocValuesField(field.fieldName, new BytesRef(value))
    )
    val facetField = Option.when(field.aggregatable && value != null && !value.isBlank)(
      new SortedSetDocValuesFacetField(field.fieldName, value)
    )

    Seq(Some(stringField), sortField, facetField).flatten
  }

  private def getFieldsForUntokenizedText(
      field: IndexField,
      value: String
  ): Seq[IndexableField] = {
    if (field.kind != FieldKind.UntokenizedText) {
      throw new WrongFieldTypeException(f"Cannot create untokenized text field for field ${field.fieldName}")
    }
    val textField = new TextField(field.fieldName, value, Store.YES)
    val sortField = Option.when(field.sortable && value != null && !value.isBlank)(
      new SortedDocValuesField(field.fieldName, new BytesRef(value))
    )
    val facetField = Option.when(field.aggregatable && value != null && !value.isBlank)(
      new SortedSetDocValuesFacetField(field.fieldName, value)
    )

    Seq(Some(textField), sortField, facetField).flatten
  }

  private def getFieldsForMultiString(
      field: IndexField,
      values: Seq[String]
  ): Seq[IndexableField] = {
    if (field.kind != FieldKind.MultiString) {
      throw new WrongFieldTypeException(f"Cannot create multi-string field for field ${field.fieldName}")
    }
    values.map(new StringField(field.fieldName, _, Store.YES))
  }

  private def getFieldsForInstant(field: IndexField, instant: Instant): Seq[IndexableField] = {
    if (field.kind != FieldKind.Instant) {
      throw new WrongFieldTypeException(f"Cannot create instant field for field ${field.fieldName}")
    }
    val dateAsLong = instant.toEpochMilli
    Seq(new LongPoint(field.fieldName, dateAsLong), new NumericDocValuesField(field.fieldName, dateAsLong))
  }

  private def getFieldsForYear(field: IndexField, numberField: IndexField, value: String): Seq[IndexableField] = {
    if (field.kind != FieldKind.UntokenizedText) {
      throw new WrongFieldTypeException(f"Cannot create year field for field ${field.fieldName}")
    }
    if (numberField.kind != FieldKind.Integer) {
      throw new WrongFieldTypeException(f"Cannot create year field for number field ${numberField.fieldName}")
    }

    val stringField = new TextField(field.fieldName, value, Store.YES)
    val numericString = value.replaceAll(raw"\D", "")
    if (numericString.length >= 3 && numericString.length <= 4) {
      val year = (if (numericString.length == 3) { numericString + "0" }
                  else { numericString }).toInt
      Seq(
        stringField,
        new IntPoint(numberField.fieldName, year),
        new NumericDocValuesField(numberField.fieldName, year)
      )
    } else {
      Seq(stringField)
    }
  }
}
