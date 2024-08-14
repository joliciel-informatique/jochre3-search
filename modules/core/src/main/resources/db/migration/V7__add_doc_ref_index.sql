CREATE INDEX IF NOT EXISTS metadata_correction_doc_ref_idx ON metadata_correction_doc(doc_ref);
CREATE INDEX IF NOT EXISTS word_suggestion_doc_ref_idx ON word_suggestion(doc_ref);

CREATE TABLE indexed_document_correction (
  reference TEXT NOT NULL,
  field TEXT NOT NULL,
  metadata_correction_rev BIGINT NOT NULL,
  PRIMARY KEY(reference, field),
  FOREIGN KEY(reference) REFERENCES indexed_document(reference) ON DELETE CASCADE
);

INSERT INTO indexed_document_correction (reference, field, metadata_correction_rev)
SELECT indexdoc.reference, mc.field, indexdoc.metadata_correction_rev
FROM indexed_document indexdoc
INNER JOIN metadata_correction mc ON indexdoc.metadata_correction_rev = mc.rev
WHERE indexdoc.metadata_correction_rev IS NOT NULL;

ALTER TABLE indexed_document DROP COLUMN metadata_correction_rev;

