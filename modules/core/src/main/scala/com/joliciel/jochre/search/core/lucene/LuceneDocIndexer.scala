package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.AltoDocument
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.{Document => IndexDocument, _}
import org.apache.lucene.facet.FacetsConfig
import org.apache.lucene.facet.sortedset.SortedSetDocValuesFacetField
import org.apache.lucene.index.{IndexWriter, IndexableField, Term}
import org.apache.lucene.util.BytesRef
import org.slf4j.LoggerFactory

import java.time.Instant

private[search] case class LuceneDocIndexer(private val indexWriter: IndexWriter) {
  private val log = LoggerFactory.getLogger(getClass)

  private val facetConfigs = new FacetsConfig {
    override def getDimConfig(dimName: String): FacetsConfig.DimConfig = {
      val config = super.getDimConfig(dimName)
      if (LuceneField.withName(dimName).isMultiValue) {
        config.multiValued = true
      }
      config
    }
  }

  def indexDocument(doc: AltoDocument): Unit = {
    val docTerm = new Term(LuceneField.Reference.entryName, doc.ref.ref)
    val fields = toLuceneDoc(doc)
    indexWriter.updateDocument(docTerm, fields)
    commit()
  }

  def commit(): Unit = {
    indexWriter.commit()
  }

  private def fieldsToLuceneDoc(fields: Seq[IndexableField]): IndexDocument = {
    val luceneDoc = new IndexDocument
    fields.foreach(luceneDoc.add(_))
    facetConfigs.build(luceneDoc)
  }

  private def toLuceneDoc(
      doc: AltoDocument
  ): IndexDocument = fieldsToLuceneDoc(
    Seq(
      getIdFields(doc),
      getFieldsForStringMeta(LuceneField.Title, doc.metadata.title, withFacet = false),
      doc.metadata.author.map(getFieldsForStringMeta(LuceneField.Author, _)).getOrElse(Seq.empty),
      getTextField(doc),
      getFieldForInstant(LuceneField.IndexTime.entryName, Instant.now())
    ).flatten
  )

  private def getTextField(doc: AltoDocument): Seq[IndexableField] = Seq(
    new StoredTextField(LuceneField.Text.entryName, doc.text)
  )
  private def getIdFields(doc: AltoDocument): Seq[IndexableField] = Seq(
    new StringField(LuceneField.Reference.entryName, doc.ref.ref, Store.YES),
    new SortedDocValuesField(LuceneField.Reference.entryName, new BytesRef(doc.ref.ref)),
    new StringField(LuceneField.Revision.entryName, doc.rev.rev.toString, Store.YES)
  )

  private def getFieldsForStringMeta(
      field: LuceneField,
      value: String,
      withFacet: Boolean = true
  ): Seq[IndexableField] = {
    val stringField = new StringField(field.entryName, value, Store.YES)
    val sortField = new SortedDocValuesField(field.entryName, new BytesRef(value))
    val facetField = Option.when(withFacet && value != null && !value.isBlank)(
      new SortedSetDocValuesFacetField(field.entryName, value)
    )

    Seq(Some(stringField), Some(sortField), facetField).flatten
  }

  private def getFieldForInstant(name: String, instant: Instant): Seq[IndexableField] = {
    val dateAsLong = instant.toEpochMilli
    Seq(new LongPoint(name, dateAsLong), new NumericDocValuesField(name, dateAsLong))
  }

}
