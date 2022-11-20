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


alter table "order" ADD column "lots" INTEGER NOT NULL DEFAULT 1;

alter table "order" alter column limit_price drop not null;
alter table "order" alter column buy_price drop not null;


alter table "order" add column buy_or_sell text default 'buy';
