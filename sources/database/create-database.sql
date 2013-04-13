
CREATE TABLE "customers"
(
	"customer-id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"first-name" TEXT,
	"last-name" TEXT,
	"is-vip" INTEGER
);

CREATE TABLE "shipping-addresses"
(
	"shipping-address-id" INTEGER PRIMARY KEY AUTOINCREMENT,
	"customer-id" INTEGER REFERENCES "customers" ("customer-id"),
	"effective-date" TEXT,
	"street" TEXT,
	"city" TEXT,
	"state" TEXT
);

CREATE INDEX "shipping-addresses-customer-id" ON "shipping-addresses" ("customer-id");
