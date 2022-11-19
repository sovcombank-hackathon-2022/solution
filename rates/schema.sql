CREATE TABLE rate_source (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE rate (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    source_id BIGINT NOT NULL,
    currency TEXT NOT NULL,
    value DECIMAL NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE INDEX rate_currency_created_at ON rate (currency, created_at);
