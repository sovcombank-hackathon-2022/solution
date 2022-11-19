CREATE TABLE account (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    currency TEXT NOT NULL,
    amount DECIMAL NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE INDEX account_user_id ON account (user_id);


CREATE TABLE operation (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    account_id BIGINT NOT NULL,
    amount DECIMAL NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE INDEX operation_account_id ON operation (account_id);
