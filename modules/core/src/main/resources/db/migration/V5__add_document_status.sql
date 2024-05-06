CREATE TYPE document_status AS ENUM ('Underway', 'Complete');

ALTER TABLE document ADD COLUMN status document_status NOT NULL DEFAULT 'Underway';

UPDATE document SET status = 'Complete';