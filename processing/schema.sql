CREATE TABLE "order" (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    user_id BIGINT NOT NULL,
    account_id BIGINT NOT NULL,
    type TEXT NOT NULL,
    status TEXT NOT NULL,
    currency TEXT NOT NULL,
    limit_price DECIMAL NOT NULL,
    buy_price DECIMAL NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE INDEX order_user_id_status ON "order" (user_id, status);
