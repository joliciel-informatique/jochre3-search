ALTER TABLE document DROP COLUMN status;
ALTER TABLE document DROP COLUMN new_suggestion_offset;

DROP TYPE index_status;

ALTER TABLE word_suggestion ADD COLUMN rev BIGSERIAL NOT NULL;
ALTER TABLE word_suggestion ADD COLUMN start_offset INT;
UPDATE word_suggestion SET start_offset=0;
ALTER TABLE word_suggestion ALTER COLUMN start_offset SET NOT NULL;

ALTER TABLE metadata_correction ADD COLUMN rev BIGSERIAL NOT NULL;

CREATE TABLE indexed_document(
  reference TEXT NOT NULL PRIMARY KEY,
  doc_rev BIGINT NOT NULL,
  word_suggestion_rev BIGINT NULL,
  metadata_correction_rev BIGINT NULL,
  reindex BOOLEAN NOT NULL DEFAULT false,
  index_time TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

INSERT INTO indexed_document (reference, doc_rev) SELECT reference, max(rev) FROM document GROUP BY reference;