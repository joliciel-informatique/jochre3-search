CREATE TYPE index_status AS ENUM ('Unindexed', 'NewMetadata', 'NewSuggestion', 'Indexed');

ALTER TABLE document DROP COLUMN reindex;

ALTER TABLE document ADD COLUMN status index_status NOT NULL DEFAULT 'Unindexed';
ALTER TABLE document ADD COLUMN new_suggestion_offset INT NULL;