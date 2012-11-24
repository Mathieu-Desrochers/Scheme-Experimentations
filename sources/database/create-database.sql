
CREATE TABLE "customer"
(
	"customer-id" INTEGER PRIMARY KEY,
	"first-name" TEXT,
	"last-name" TEXT
);

CREATE TABLE "customer-address"
(
	"customer-address-id" INTEGER PRIMARY KEY,
	"customer-id" INTEGER,
	"address" TEXT,
	"city" TEXT,
	"state" TEXT
);
