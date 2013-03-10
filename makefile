
make : compile link

compile : compile-bindings \
          compile-core \
          compile-foreign-interfaces \
          compile-infrastructure

compile-bindings : compile-bindings-http

compile-bindings-http : sources/bindings/http/new-customer-service.o

sources/bindings/http/new-customer-service.o : sources/bindings/http/new-customer-service.scm
	csc -c -extend sources/macros/bindings/http/define-http-binding.scm \
	sources/bindings/http/new-customer-service.scm -o \
	sources/bindings/http/new-customer-service.o

compile-core : compile-core-services \
               compile-core-tables

compile-core-services : sources/core/services/new-customer-service.o

sources/core/services/new-customer-service.o : sources/core/services/new-customer-service.scm
	csc -c \
	-extend sources/macros/core/services/define-request.scm \
	-extend sources/macros/core/services/define-response.scm \
	sources/core/services/new-customer-service.scm -o \
	sources/core/services/new-customer-service.o

compile-core-tables : sources/core/tables/customer-addresses-table.o \
                      sources/core/tables/customers-table.o

sources/core/tables/customer-addresses-table.o : sources/core/tables/customer-addresses-table.scm
	csc -c -extend sources/macros/core/tables/define-table.scm \
	sources/core/tables/customer-addresses-table.scm -o \
	sources/core/tables/customer-addresses-table.o

sources/core/tables/customers-table.o : sources/core/tables/customers-table.scm
	csc -c -extend sources/macros/core/tables/define-table.scm \
	sources/core/tables/customers-table.scm -o \
	sources/core/tables/customers-table.o

compile-foreign-interfaces : compile-foreign-interfaces-fastcgi \
                             compile-foreign-interfaces-jansson \
                             compile-foreign-interfaces-sqlite

compile-foreign-interfaces-fastcgi : sources/foreign-interfaces/fastcgi.o

sources/foreign-interfaces/fastcgi.o : sources/foreign-interfaces/fastcgi.scm
	csc -c -I/usr/local/include \
	sources/foreign-interfaces/fastcgi.scm -o \
	sources/foreign-interfaces/fastcgi.o

compile-foreign-interfaces-jansson : sources/foreign-interfaces/jansson.o

sources/foreign-interfaces/jansson.o : sources/foreign-interfaces/jansson.scm
	csc -c -I/usr/local/include \
	sources/foreign-interfaces/jansson.scm -o \
	sources/foreign-interfaces/jansson.o

compile-foreign-interfaces-sqlite : sources/foreign-interfaces/sqlite.o

sources/foreign-interfaces/sqlite.o : sources/foreign-interfaces/sqlite.scm
	csc -c -I/usr/local/include \
	sources/foreign-interfaces/sqlite.scm -o \
	sources/foreign-interfaces/sqlite.o

compile-infrastructure : compile-infrastructure-exceptions \
                         compile-infrastructure-http \
                         compile-infrastructure-json \
                         compile-infrastructure-services \
                         compile-infrastructure-sql \
                         compile-infrastructure-validation

compile-infrastructure-exceptions : sources/infrastructure/exceptions/exceptions.o

sources/infrastructure/exceptions/exceptions.o : sources/infrastructure/exceptions/exceptions.scm
	csc -c \
	sources/infrastructure/exceptions/exceptions.scm -o \
	sources/infrastructure/exceptions/exceptions.o

compile-infrastructure-http : sources/infrastructure/http/http.o \
                              sources/infrastructure/http/http-toplevel.o \
                              sources/infrastructure/http/main.o

sources/infrastructure/http/http.o : sources/infrastructure/http/http.scm
	csc -c \
	sources/infrastructure/http/http.scm -o \
	sources/infrastructure/http/http.o

sources/infrastructure/http/http-toplevel.o : sources/infrastructure/http/http-toplevel.scm
	csc -c -e \
	sources/infrastructure/http/http-toplevel.scm -o \
	sources/infrastructure/http/http-toplevel.o

sources/infrastructure/http/main.o : sources/infrastructure/http/main.c
	csc -c \
	sources/infrastructure/http/main.c -o \
	sources/infrastructure/http/main.o

compile-infrastructure-json : sources/infrastructure/json/json.o \
                              sources/infrastructure/json/json-format.o \
                              sources/infrastructure/json/json-parse.o

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

compile-infrastructure-services : sources/infrastructure/services/services.o

sources/infrastructure/services/services.o : sources/infrastructure/services/services.scm
	csc -c \
	sources/infrastructure/services/services.scm -o \
	sources/infrastructure/services/services.o

compile-infrastructure-sql : sources/infrastructure/sql/sql-intern.o \
                             sources/infrastructure/sql/sql.o

sources/infrastructure/sql/sql-intern.o : sources/infrastructure/sql/sql-intern.scm
	csc -c \
	sources/infrastructure/sql/sql-intern.scm -o \
	sources/infrastructure/sql/sql-intern.o

sources/infrastructure/sql/sql.o : sources/infrastructure/sql/sql.scm
	csc -c \
	sources/infrastructure/sql/sql.scm -o \
	sources/infrastructure/sql/sql.o

compile-infrastructure-validation : sources/infrastructure/validation/validation.o

sources/infrastructure/validation/validation.o : sources/infrastructure/validation/validation.scm
	csc -c \
	sources/infrastructure/validation/validation.scm -o \
	sources/infrastructure/validation/validation.o

link : compile
	csc \
	-lfcgi \
	-ljansson \
	-lsqlite3 \
	sources/bindings/http/new-customer-service.o \
	sources/core/services/new-customer-service.o \
	sources/core/tables/customer-addresses-table.o \
	sources/core/tables/customers-table.o \
	sources/foreign-interfaces/fastcgi.o \
	sources/foreign-interfaces/jansson.o \
	sources/foreign-interfaces/sqlite.o \
	sources/infrastructure/exceptions/exceptions.o \
	sources/infrastructure/http/http.o \
	sources/infrastructure/http/http-toplevel.o \
	sources/infrastructure/http/main.o \
	sources/infrastructure/json/json.o \
	sources/infrastructure/json/json-format.o \
	sources/infrastructure/json/json-parse.o \
	sources/infrastructure/services/services.o \
	sources/infrastructure/sql/sql-intern.o \
	sources/infrastructure/sql/sql.o \
	sources/infrastructure/validation/validation.o \
	-o scheme

install :
	cp scheme /usr/local/apache2/fcgi-bin/
