ALTER TABLE indexed_document DROP COLUMN reindex;

CREATE TYPE reindex_type AS ENUM ('No', 'Yes', 'MetadataOnly');

ALTER TABLE indexed_document ADD COLUMN reindex reindex_type NOT NULL DEFAULT 'No';