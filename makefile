
make : compile link

compile : compile-bindings \
          compile-core \
          compile-foreign-interfaces \
          compile-infrastructure

compile-bindings : compile-bindings-http

compile-bindings-http : sources/bindings/http/new-customer-service.o \
                        sources/bindings/http/new-shipping-address-service.o

sources/bindings/http/new-customer-service.o : sources/bindings/http/new-customer-service.scm
	csc -c -extend sources/macros/bindings/http/define-http-binding.scm \
	sources/bindings/http/new-customer-service.scm -o \
	sources/bindings/http/new-customer-service.o

sources/bindings/http/new-shipping-address-service.o : sources/bindings/http/new-shipping-address-service.scm
	csc -c -extend sources/macros/bindings/http/define-http-binding.scm \
	sources/bindings/http/new-shipping-address-service.scm -o \
	sources/bindings/http/new-shipping-address-service.o

compile-core : compile-core-services \
               compile-core-tables

compile-core-services : sources/core/services/new-customer-service.o \
                        sources/core/services/new-shipping-address-service.o

sources/core/services/new-customer-service.o : sources/core/services/new-customer-service.scm
	csc -c \
	-extend sources/macros/core/services/define-request.scm \
	-extend sources/macros/core/services/define-response.scm \
	sources/core/services/new-customer-service.scm -o \
	sources/core/services/new-customer-service.o

sources/core/services/new-shipping-address-service.o : sources/core/services/new-shipping-address-service.scm
	csc -c \
	-extend sources/macros/core/services/define-request.scm \
	-extend sources/macros/core/services/define-response.scm \
	sources/core/services/new-shipping-address-service.scm -o \
	sources/core/services/new-shipping-address-service.o

compile-core-tables : sources/core/tables/customers-table.o \
                      sources/core/tables/shipping-addresses-table.o

sources/core/tables/customers-table.o : sources/core/tables/customers-table.scm
	csc -c -extend sources/macros/core/tables/define-table.scm \
	sources/core/tables/customers-table.scm -o \
	sources/core/tables/customers-table.o

sources/core/tables/shipping-addresses-table.o : sources/core/tables/shipping-addresses-table.scm
	csc -c -extend sources/macros/core/tables/define-table.scm \
	sources/core/tables/shipping-addresses-table.scm -o \
	sources/core/tables/shipping-addresses-table.o

compile-foreign-interfaces : compile-foreign-interfaces-fastcgi \
                             compile-foreign-interfaces-jansson \
                             compile-foreign-interfaces-pcre \
                             compile-foreign-interfaces-scdtl \
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

compile-foreign-interfaces-pcre : sources/foreign-interfaces/pcre.o

sources/foreign-interfaces/pcre.o : sources/foreign-interfaces/pcre.scm
	csc -c -I/usr/local/include \
	sources/foreign-interfaces/pcre.scm -o \
	sources/foreign-interfaces/pcre.o

compile-foreign-interfaces-scdtl : sources/foreign-interfaces/scdtl.o

sources/foreign-interfaces/scdtl.o : sources/foreign-interfaces/scdtl.scm
	csc -C "-D_XOPEN_SOURCE" -c \
	sources/foreign-interfaces/scdtl.scm -o \
	sources/foreign-interfaces/scdtl.o

compile-foreign-interfaces-sqlite : sources/foreign-interfaces/sqlite.o

sources/foreign-interfaces/sqlite.o : sources/foreign-interfaces/sqlite.scm
	csc -c -I/usr/local/include \
	sources/foreign-interfaces/sqlite.scm -o \
	sources/foreign-interfaces/sqlite.o

compile-infrastructure : compile-infrastructure-datetime \
                         compile-infrastructure-exceptions \
                         compile-infrastructure-http \
                         compile-infrastructure-json \
                         compile-infrastructure-regex \
                         compile-infrastructure-services \
                         compile-infrastructure-sql \
                         compile-infrastructure-validation

compile-infrastructure-datetime : sources/infrastructure/datetime/datetime.o \
                                  sources/infrastructure/datetime/datetime-intern.o

sources/infrastructure/datetime/datetime.o : sources/infrastructure/datetime/datetime.scm
	csc -c \
	sources/infrastructure/datetime/datetime.scm -o \
	sources/infrastructure/datetime/datetime.o

sources/infrastructure/datetime/datetime-intern.o : sources/infrastructure/datetime/datetime-intern.scm
	csc -c \
	sources/infrastructure/datetime/datetime-intern.scm -o \
	sources/infrastructure/datetime/datetime-intern.o

compile-infrastructure-exceptions : sources/infrastructure/exceptions/exceptions.o

sources/infrastructure/exceptions/exceptions.o : sources/infrastructure/exceptions/exceptions.scm
	csc -c \
	sources/infrastructure/exceptions/exceptions.scm -o \
	sources/infrastructure/exceptions/exceptions.o

compile-infrastructure-http : sources/infrastructure/http/http.o \
                              sources/infrastructure/http/http-bindings.o \
                              sources/infrastructure/http/http-toplevel.o \
                              sources/infrastructure/http/main.o

sources/infrastructure/http/http.o : sources/infrastructure/http/http.scm
	csc -c \
	sources/infrastructure/http/http.scm -o \
	sources/infrastructure/http/http.o

sources/infrastructure/http/http-bindings.o : sources/infrastructure/http/http-bindings.scm
	csc -c \
	sources/infrastructure/http/http-bindings.scm -o \
	sources/infrastructure/http/http-bindings.o

sources/infrastructure/http/http-toplevel.o : sources/infrastructure/http/http-toplevel.scm
	csc -c -e \
	sources/infrastructure/http/http-toplevel.scm -o \
	sources/infrastructure/http/http-toplevel.o

sources/infrastructure/http/main.o : sources/infrastructure/http/main.c
	csc -c \
	sources/infrastructure/http/main.c -o \
	sources/infrastructure/http/main.o

compile-infrastructure-json : sources/infrastructure/json/json.o \
                              sources/infrastructure/json/json-convert.o \
                              sources/infrastructure/json/json-format.o \
                              sources/infrastructure/json/json-parse.o

sources/infrastructure/json/json.o : sources/infrastructure/json/json.scm
	csc -c \
	sources/infrastructure/json/json.scm -o \
	sources/infrastructure/json/json.o

sources/infrastructure/json/json-convert.o : sources/infrastructure/json/json-convert.scm
	csc -c \
	sources/infrastructure/json/json-convert.scm -o \
	sources/infrastructure/json/json-convert.o

sources/infrastructure/json/json-format.o : sources/infrastructure/json/json-format.scm
	csc -c \
	sources/infrastructure/json/json-format.scm -o \
	sources/infrastructure/json/json-format.o

sources/infrastructure/json/json-parse.o : sources/infrastructure/json/json-parse.scm
	csc -c \
	sources/infrastructure/json/json-parse.scm -o \
	sources/infrastructure/json/json-parse.o

compile-infrastructure-regex : sources/infrastructure/regex/regex.o \
                               sources/infrastructure/regex/regex-intern.o

sources/infrastructure/regex/regex.o : sources/infrastructure/regex/regex.scm
	csc -c \
	sources/infrastructure/regex/regex.scm -o \
	sources/infrastructure/regex/regex.o

sources/infrastructure/regex/regex-intern.o : sources/infrastructure/regex/regex-intern.scm
	csc -c \
	sources/infrastructure/regex/regex-intern.scm -o \
	sources/infrastructure/regex/regex-intern.o

compile-infrastructure-services : sources/infrastructure/services/services.o

sources/infrastructure/services/services.o : sources/infrastructure/services/services.scm
	csc -c \
	sources/infrastructure/services/services.scm -o \
	sources/infrastructure/services/services.o

compile-infrastructure-sql : sources/infrastructure/sql/sql.o \
                             sources/infrastructure/sql/sql-convert.o \
                             sources/infrastructure/sql/sql-intern.o

sources/infrastructure/sql/sql.o : sources/infrastructure/sql/sql.scm
	csc -c \
	sources/infrastructure/sql/sql.scm -o \
	sources/infrastructure/sql/sql.o

sources/infrastructure/sql/sql-convert.o : sources/infrastructure/sql/sql-convert.scm
	csc -c \
	sources/infrastructure/sql/sql-convert.scm -o \
	sources/infrastructure/sql/sql-convert.o

sources/infrastructure/sql/sql-intern.o : sources/infrastructure/sql/sql-intern.scm
	csc -c \
	sources/infrastructure/sql/sql-intern.scm -o \
	sources/infrastructure/sql/sql-intern.o

compile-infrastructure-validation : sources/infrastructure/validation/validation.o

sources/infrastructure/validation/validation.o : sources/infrastructure/validation/validation.scm
	csc -c \
	sources/infrastructure/validation/validation.scm -o \
	sources/infrastructure/validation/validation.o

link : compile
	csc \
	-lfcgi \
	-ljansson \
	-lpcre \
	-lsqlite3 \
	sources/bindings/http/new-customer-service.o \
	sources/bindings/http/new-shipping-address-service.o \
	sources/core/services/new-customer-service.o \
	sources/core/services/new-shipping-address-service.o \
	sources/core/tables/customers-table.o \
	sources/core/tables/shipping-addresses-table.o \
	sources/foreign-interfaces/fastcgi.o \
	sources/foreign-interfaces/jansson.o \
	sources/foreign-interfaces/pcre.o \
	sources/foreign-interfaces/scdtl.o \
	sources/foreign-interfaces/sqlite.o \
	sources/infrastructure/datetime/datetime.o \
	sources/infrastructure/datetime/datetime-intern.o \
	sources/infrastructure/exceptions/exceptions.o \
	sources/infrastructure/http/http.o \
	sources/infrastructure/http/http-bindings.o \
	sources/infrastructure/http/http-toplevel.o \
	sources/infrastructure/http/main.o \
	sources/infrastructure/json/json.o \
	sources/infrastructure/json/json-convert.o \
	sources/infrastructure/json/json-format.o \
	sources/infrastructure/json/json-parse.o \
	sources/infrastructure/regex/regex.o \
	sources/infrastructure/regex/regex-intern.o \
	sources/infrastructure/services/services.o \
	sources/infrastructure/sql/sql.o \
	sources/infrastructure/sql/sql-convert.o \
	sources/infrastructure/sql/sql-intern.o \
	sources/infrastructure/validation/validation.o \
	-o scheme

install :
	cp scheme /usr/local/apache2/fcgi-bin/
