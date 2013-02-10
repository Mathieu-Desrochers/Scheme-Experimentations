
make : compile link

compile : infrastructure application

infrastructure : http jansson validation sql

http : sources/infrastructure/http/http.o \
	   sources/infrastructure/http/http-toplevel.o \
	   sources/infrastructure/http/fastcgi.o \
	   sources/infrastructure/http/fastcgi-ffi.o

sources/infrastructure/http/http.o : sources/infrastructure/http/http.scm
	csc -c \
	sources/infrastructure/http/http.scm -o \
	sources/infrastructure/http/http.o

sources/infrastructure/http/http-toplevel.o : sources/infrastructure/http/http-toplevel.scm
	csc -c -e \
	sources/infrastructure/http/http-toplevel.scm -o \
	sources/infrastructure/http/http-toplevel.o

sources/infrastructure/http/fastcgi.o : sources/infrastructure/http/fastcgi.c
	csc -c \
	sources/infrastructure/http/fastcgi.c -o \
	sources/infrastructure/http/fastcgi.o

sources/infrastructure/http/fastcgi-ffi.o : sources/infrastructure/http/fastcgi-ffi.scm
	csc -c -I/usr/local/include \
	sources/infrastructure/http/fastcgi-ffi.scm -o \
	sources/infrastructure/http/fastcgi-ffi.o

jansson : sources/infrastructure/json/jansson-ffi.o \
          sources/infrastructure/json/json.o \
          sources/infrastructure/json/json-format.o \
          sources/infrastructure/json/json-parse.o

sources/infrastructure/json/jansson-ffi.o : sources/infrastructure/json/jansson-ffi.scm
	csc -c -I/usr/local/include \
	sources/infrastructure/json/jansson-ffi.scm -o \
	sources/infrastructure/json/jansson-ffi.o

sources/infrastructure/json/json.o : sources/infrastructure/json/json.scm
	csc -c \
	sources/infrastructure/json/json.scm -o \
	sources/infrastructure/json/json.o

sources/infrastructure/json/json-format.o : sources/infrastructure/json/json-format.scm
	csc -c \
	sources/infrastructure/json/json-format.scm -o \
	sources/infrastructure/json/json-format.o

sources/infrastructure/json/json-parse.o : sources/infrastructure/json/json-parse.scm
	csc -c \
	sources/infrastructure/json/json-parse.scm -o \
	sources/infrastructure/json/json-parse.o

validation : sources/infrastructure/validation/validation.o

sources/infrastructure/validation/validation.o : sources/infrastructure/validation/validation.scm
	csc -c \
	sources/infrastructure/validation/validation.scm -o \
	sources/infrastructure/validation/validation.o

sql : sources/infrastructure/sql/sqlite-ffi.o \
      sources/infrastructure/sql/sql-intern.o \
      sources/infrastructure/sql/sql.o

sources/infrastructure/sql/sqlite-ffi.o : sources/infrastructure/sql/sqlite-ffi.scm
	csc -c -I/usr/local/include \
	sources/infrastructure/sql/sqlite-ffi.scm -o \
	sources/infrastructure/sql/sqlite-ffi.o

sources/infrastructure/sql/sql-intern.o : sources/infrastructure/sql/sql-intern.scm
	csc -c \
	sources/infrastructure/sql/sql-intern.scm -o \
	sources/infrastructure/sql/sql-intern.o

sources/infrastructure/sql/sql.o : sources/infrastructure/sql/sql.scm
	csc -c \
	sources/infrastructure/sql/sql.scm -o \
	sources/infrastructure/sql/sql.o

application : services tables

services : sources/application/services/new-customer-service.o

sources/application/services/new-customer-service.o : sources/application/services/new-customer-service.scm
	csc -c \
	-extend sources/infrastructure/services/define-request.scm \
	-extend sources/infrastructure/services/define-response.scm \
	sources/application/services/new-customer-service.scm -o \
	sources/application/services/new-customer-service.o

tables : sources/application/tables/customer-addresses-table.o \
         sources/application/tables/customers-table.o

sources/application/tables/customer-addresses-table.o : sources/application/tables/customer-addresses-table.scm
	csc -c -extend sources/infrastructure/tables/define-table.scm \
	sources/application/tables/customer-addresses-table.scm -o \
	sources/application/tables/customer-addresses-table.o

sources/application/tables/customers-table.o : sources/application/tables/customers-table.scm
	csc -c -extend sources/infrastructure/tables/define-table.scm \
	sources/application/tables/customers-table.scm -o \
	sources/application/tables/customers-table.o

link : compile
	csc \
	-lfcgi \
	-ljansson \
	-lsqlite3 \
	sources/infrastructure/http/http.o \
	sources/infrastructure/http/http-toplevel.o \
	sources/infrastructure/http/fastcgi.o \
	sources/infrastructure/http/fastcgi-ffi.o \
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
	-o scheme

install :
	cp scheme /usr/local/apache2/fcgi-bin/
