
CREATE TABLE "customers"
(
	"customer-id" INTEGER PRIMARY KEY,
	"first-name" TEXT,
	"last-name" TEXT,
	"is-vip" INTEGER
);

CREATE TABLE "shipping-addresses"
(
	"shipping-address-id" INTEGER PRIMARY KEY,
	"customer-id" INTEGER,
	"effective-date" TEXT,
	"street" TEXT,
	"city" TEXT,
	"state" TEXT
);
