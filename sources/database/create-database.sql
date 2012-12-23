
CREATE TABLE "customers"
(
	"customer-id" INTEGER PRIMARY KEY,
	"first-name" TEXT,
	"last-name" TEXT
);

CREATE TABLE "customer-addresses"
(
	"customer-address-id" INTEGER PRIMARY KEY,
	"customer-id" INTEGER,
	"address" TEXT,
	"city" TEXT,
	"state" TEXT
);
