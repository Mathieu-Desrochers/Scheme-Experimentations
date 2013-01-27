
make : compile link

compile : infrastructure application

infrastructure : httpd jansson validation sql

httpd : httpd-ffi.o mod_scheme.o

httpd-ffi.o : sources/infrastructure/httpd/httpd-ffi.scm
	csc -t -e sources/infrastructure/httpd/httpd-ffi.scm
	gcc -c -fPIC -DC_EMBEDDED \
	-I/usr/local/include/chicken \
	-I/usr/local/apr/include/apr-1 \
	-I/usr/local/apache2/include \
	sources/infrastructure/httpd/httpd-ffi.c -o \
	sources/infrastructure/httpd/httpd-ffi.o

mod_scheme.o : sources/infrastructure/httpd/mod_scheme.c
	gcc -c -fPIC -DC_EMBEDDED \
	-I/usr/local/include/chicken \
	-I/usr/local/apr/include/apr-1 \
	-I/usr/local/apache2/include \
	sources/infrastructure/httpd/mod_scheme.c -o \
	sources/infrastructure/httpd/mod_scheme.o

jansson : sources/infrastructure/json/jansson-ffi.o \
          sources/infrastructure/json/json.o \
          sources/infrastructure/json/json-format.o \
          sources/infrastructure/json/json-parse.o

sources/infrastructure/json/jansson-ffi.o : sources/infrastructure/json/jansson-ffi.scm
	csc -t sources/infrastructure/json/jansson-ffi.scm
	gcc -c -fPIC -DC_EMBEDDED \
	-I/usr/local/include/chicken \
	-I/usr/local/include \
	sources/infrastructure/json/jansson-ffi.c -o \
	sources/infrastructure/json/jansson-ffi.o

sources/infrastructure/json/json.o : sources/infrastructure/json/json.scm
	csc -t sources/infrastructure/json/json.scm
	gcc -c -fPIC -DC_EMBEDDED -I/usr/local/include/chicken \
	sources/infrastructure/json/json.c -o \
	sources/infrastructure/json/json.o

sources/infrastructure/json/json-format.o : sources/infrastructure/json/json-format.scm
	csc -t sources/infrastructure/json/json-format.scm
	gcc -c -fPIC -DC_EMBEDDED -I/usr/local/include/chicken \
	sources/infrastructure/json/json-format.c -o \
	sources/infrastructure/json/json-format.o

sources/infrastructure/json/json-parse.o : sources/infrastructure/json/json-parse.scm
	csc -t sources/infrastructure/json/json-parse.scm
	gcc -c -fPIC -DC_EMBEDDED -I/usr/local/include/chicken \
	sources/infrastructure/json/json-parse.c -o \
	sources/infrastructure/json/json-parse.o

validation : sources/infrastructure/validation/validation.o

sources/infrastructure/validation/validation.o : sources/infrastructure/validation/validation.scm
	csc -t sources/infrastructure/validation/validation.scm
	gcc -c -fPIC -DC_EMBEDDED -I/usr/local/include/chicken \
	sources/infrastructure/validation/validation.c -o \
	sources/infrastructure/validation/validation.o

sql : sources/infrastructure/json/sqlite-ffi.o \
      sources/infrastructure/json/sql-intern.o \
      sources/infrastructure/json/sql.o

sources/infrastructure/json/sqlite-ffi.o : sources/infrastructure/sql/sqlite-ffi.scm
	csc -t sources/infrastructure/sql/sqlite-ffi.scm
	gcc -c -fPIC -DC_EMBEDDED  \
	-I/usr/local/include/chicken \
	-I/usr/local/include \
	sources/infrastructure/sql/sqlite-ffi.c -o \
	sources/infrastructure/sql/sqlite-ffi.o

sources/infrastructure/json/sql-intern.o : sources/infrastructure/sql/sql-intern.scm
	csc -t sources/infrastructure/sql/sql-intern.scm
	gcc -c -fPIC -DC_EMBEDDED -I/usr/local/include/chicken \
	sources/infrastructure/sql/sql-intern.c -o \
	sources/infrastructure/sql/sql-intern.o

sources/infrastructure/json/sql.o : sources/infrastructure/sql/sql.scm
	csc -t sources/infrastructure/sql/sql.scm
	gcc -c -fPIC -DC_EMBEDDED -I/usr/local/include/chicken \
	sources/infrastructure/sql/sql.c -o \
	sources/infrastructure/sql/sql.o

application : services tables

services : sources/application/services/new-customer-service.o

sources/application/services/new-customer-service.o : sources/application/services/new-customer-service.scm
	csc -t -extend sources/infrastructure/services/define-request.scm \
	-extend sources/infrastructure/services/define-response.scm \
	sources/application/services/new-customer-service.scm
	gcc -c -fPIC -DC_EMBEDDED -I/usr/local/include/chicken \
	sources/application/services/new-customer-service.c -o \
	sources/application/services/new-customer-service.o

tables : sources/application/tables/customer-addresses-table.o \
         sources/application/tables/customers-table.o

sources/application/tables/customer-addresses-table.o : sources/application/tables/customer-addresses-table.scm
	csc -t -extend sources/infrastructure/tables/define-table.scm \
	sources/application/tables/customer-addresses-table.scm
	gcc -c -fPIC -DC_EMBEDDED -I/usr/local/include/chicken \
	sources/application/tables/customer-addresses-table.c -o \
	sources/application/tables/customer-addresses-table.o

sources/application/tables/customers-table.o : sources/application/tables/customers-table.scm
	csc -t -extend sources/infrastructure/tables/define-table.scm \
	sources/application/tables/customers-table.scm
	gcc -c -fPIC -DC_EMBEDDED -I/usr/local/include/chicken \
	sources/application/tables/customers-table.c -o \
	sources/application/tables/customers-table.o

link : compile
	/usr/local/apache2/bin/apxs -c \
	-lchicken \
	-ljansson \
	-lsqlite3 \
	sources/infrastructure/httpd/mod_scheme.o \
	sources/infrastructure/httpd/httpd-ffi.o \
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
	sources/application/tables/customers-table.o

install :
	/usr/local/apache2/bin/apxs -i -a \
	sources/infrastructure/httpd/mod_scheme.la
