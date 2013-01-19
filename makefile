
scheme-experimentation : compilation linking

compilation : infrastructure application

infrastructure : jansson validation  sql

jansson : sources/infrastructure/json/jansson-ffi.o \
          sources/infrastructure/json/json.o \
          sources/infrastructure/json/json-format.o \
          sources/infrastructure/json/json-parse.o

sources/infrastructure/json/jansson-ffi.o : sources/infrastructure/json/jansson-ffi.scm
	csc -I/usr/local/include -c sources/infrastructure/json/jansson-ffi.scm

sources/infrastructure/json/json.o : sources/infrastructure/json/json.scm
	csc -c sources/infrastructure/json/json.scm

sources/infrastructure/json/json-format.o : sources/infrastructure/json/json-format.scm
	csc -c sources/infrastructure/json/json-format.scm

sources/infrastructure/json/json-parse.o : sources/infrastructure/json/json-parse.scm
	csc -c sources/infrastructure/json/json-parse.scm

validation : sources/infrastructure/validation/validation.o

sources/infrastructure/validation/validation.o : sources/infrastructure/validation/validation.scm
	csc -c sources/infrastructure/validation/validation.scm

sql : sources/infrastructure/json/sqlite-ffi.o \
      sources/infrastructure/json/sql-intern.o \
      sources/infrastructure/json/sql.o

sources/infrastructure/json/sqlite-ffi.o : sources/infrastructure/sql/sqlite-ffi.scm
	csc -I/usr/local/include -c sources/infrastructure/sql/sqlite-ffi.scm

sources/infrastructure/json/sql-intern.o : sources/infrastructure/sql/sql-intern.scm
	csc -c sources/infrastructure/sql/sql-intern.scm

sources/infrastructure/json/sql.o : sources/infrastructure/sql/sql.scm
	csc -c sources/infrastructure/sql/sql.scm

application : services tables main

services : sources/application/services/new-customer-service.o

sources/application/services/new-customer-service.o : sources/application/services/new-customer-service.scm
	csc -c -extend sources/infrastructure/services/define-request.scm \
	-extend sources/infrastructure/services/define-response.scm \
	sources/application/services/new-customer-service.scm

tables : sources/application/tables/customer-addresses-table.o \
         sources/application/tables/customers-table.o

sources/application/tables/customer-addresses-table.o : sources/application/tables/customer-addresses-table.scm
	csc -c -extend sources/infrastructure/tables/define-table.scm \
	sources/application/tables/customer-addresses-table.scm

sources/application/tables/customers-table.o : sources/application/tables/customers-table.scm
	csc -c -extend sources/infrastructure/tables/define-table.scm \
	sources/application/tables/customers-table.scm

main : sources/application/main.o

sources/application/main.o : sources/application/main.scm
	csc -c sources/application/main.scm

linking : compilation
	csc -ljansson \
	-lsqlite3 \
	sources/infrastructure/json/jansson-ffi.o \
	sources/infrastructure/json/json.o \
	sources/infrastructure/json/json-format.o \
	sources/infrastructure/json/json-parse.o \
	sources/infrastructure/sql/sqlite-ffi.o \
	sources/infrastructure/sql/sql-intern.o \
	sources/infrastructure/sql/sql.o \
	sources/infrastructure/validation/validation.o \
	sources/application/services/new-customer-service.o \
	sources/application/tables/customer-addresses-table.o \
	sources/application/tables/customers-table.o \
	sources/application/main.o \
	-o build
	
