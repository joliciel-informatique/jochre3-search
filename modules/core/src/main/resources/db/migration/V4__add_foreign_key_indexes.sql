CREATE INDEX IF NOT EXISTS word_row_id_idx ON word(row_id);
CREATE INDEX IF NOT EXISTS word_doc_rev_idx ON word(doc_rev);
CREATE INDEX IF NOT EXISTS row_page_id_idx ON row(page_id);
CREATE INDEX IF NOT EXISTS page_doc_rev_idx ON page(doc_rev);
