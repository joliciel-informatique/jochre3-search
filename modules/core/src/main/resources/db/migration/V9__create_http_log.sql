CREATE TABLE http_request(
  id BIGSERIAL PRIMARY KEY,
  username TEXT NOT NULL,
  ip TEXT NULL,
  created TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
  method_and_url TEXT NOT NULL,
  querystring TEXT NOT NULL
);