ALTER TYPE document_status ADD VALUE 'Indexed';
ALTER TYPE document_status ADD VALUE 'Failed';

ALTER TABLE document ADD COLUMN failure_reason text NULL;
ALTER TABLE document ADD COLUMN status_updated TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL;