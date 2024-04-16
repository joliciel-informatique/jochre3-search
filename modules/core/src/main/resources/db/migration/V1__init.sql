CREATE TABLE document(
  rev BIGSERIAL PRIMARY KEY,
  reference TEXT NOT NULL,
  created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE page(
  id BIGSERIAL PRIMARY KEY,
  doc_rev BIGINT NOT NULL,
  index SMALLINT NOT NULL,
  width SMALLINT NOT NULL,
  height SMALLINT NOT NULL,
  start_offset INT NOT NULL,
  FOREIGN KEY(doc_rev) REFERENCES document(rev),
  CONSTRAINT uk_page UNIQUE (doc_rev, index)
);

CREATE TABLE row(
  id BIGSERIAL PRIMARY KEY,
  page_id BIGINT NOT NULL,
  index SMALLINT NOT NULL,
  lft SMALLINT NOT NULL,
  top SMALLINT NOT NULL,
  width SMALLINT NOT NULL,
  height SMALLINT NOT NULL,
  FOREIGN KEY(page_id) REFERENCES page(id),
  CONSTRAINT uk_row UNIQUE (page_id, index)
);

CREATE TABLE word(
  id BIGSERIAL PRIMARY KEY,
  doc_rev BIGINT NOT NULL,
  row_id BIGINT NOT NULL,
  start_offset INT NOT NULL,
  end_offset INT NOT NULL,
  lft SMALLINT NOT NULL,
  top SMALLINT NOT NULL,
  width SMALLINT NOT NULL,
  height SMALLINT NOT NULL,
  hyphenated_offset INT NULL,
  FOREIGN KEY(doc_rev) REFERENCES document(rev),
  FOREIGN KEY(row_id) REFERENCES row(id),
  CONSTRAINT uk_word UNIQUE (doc_rev, start_offset)
);

CREATE TABLE preferences (
  username TEXT NOT NULL,
  key TEXT NOT NULL,
  preference json NOT NULL,
  PRIMARY KEY(username, key)
);

CREATE TABLE query(
  id BIGSERIAL PRIMARY KEY,
  username TEXT NOT NULL,
  executed TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
  criteria JSONB NOT NULL,
  query TEXT NULL,
  sort JSONB NOT NULL,
  first_result INT NOT NULL,
  max_result INT NOT NULL,
  result_count INT NOT NULL
);