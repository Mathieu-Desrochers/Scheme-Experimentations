
make : compile ctags link

compile : compile-bindings \
          compile-core \
          compile-foreign-interfaces \
          compile-infrastructure \
          compile-tests

compile-bindings : compile-bindings-http

compile-bindings-http : sources/bindings/http/delete-customer-service-http-binding.o \
                        sources/bindings/http/get-customer-service-http-binding.o \
                        sources/bindings/http/get-customers-service-http-binding.o \
                        sources/bindings/http/new-customer-service-http-binding.o \
                        sources/bindings/http/update-customer-service-http-binding.o

sources/bindings/http/delete-customer-service-http-binding.o : sources/bindings/http/delete-customer-service-http-binding.scm
	csc -c -extend macros/bindings/http/define-http-binding.scm \
	sources/bindings/http/delete-customer-service-http-binding.scm -o \
	sources/bindings/http/delete-customer-service-http-binding.o

sources/bindings/http/get-customer-service-http-binding.o : sources/bindings/http/get-customer-service-http-binding.scm
	csc -c -extend macros/bindings/http/define-http-binding.scm \
	sources/bindings/http/get-customer-service-http-binding.scm -o \
	sources/bindings/http/get-customer-service-http-binding.o

sources/bindings/http/get-customers-service-http-binding.o : sources/bindings/http/get-customers-service-http-binding.scm
	csc -c -extend macros/bindings/http/define-http-binding.scm \
	sources/bindings/http/get-customers-service-http-binding.scm -o \
	sources/bindings/http/get-customers-service-http-binding.o

sources/bindings/http/new-customer-service-http-binding.o : sources/bindings/http/new-customer-service-http-binding.scm
	csc -c -extend macros/bindings/http/define-http-binding.scm \
	sources/bindings/http/new-customer-service-http-binding.scm -o \
	sources/bindings/http/new-customer-service-http-binding.o

sources/bindings/http/update-customer-service-http-binding.o : sources/bindings/http/update-customer-service-http-binding.scm
	csc -c -extend macros/bindings/http/define-http-binding.scm \
	sources/bindings/http/update-customer-service-http-binding.scm -o \
	sources/bindings/http/update-customer-service-http-binding.o

compile-core : compile-core-services \
               compile-core-tables

compile-core-services : sources/core/services/delete-customer-service.o \
                        sources/core/services/get-customer-service.o \
                        sources/core/services/get-customers-service.o \
                        sources/core/services/new-customer-service.o \
                        sources/core/services/update-customer-service.o

sources/core/services/delete-customer-service.o : sources/core/services/delete-customer-service.scm
	csc -c \
	-extend macros/core/services/define-request.scm \
	-extend macros/core/services/define-response.scm \
	-extend macros/core/services/structure.scm \
	sources/core/services/delete-customer-service.scm -o \
	sources/core/services/delete-customer-service.o

sources/core/services/get-customer-service.o : sources/core/services/get-customer-service.scm
	csc -c \
	-extend macros/core/services/define-request.scm \
	-extend macros/core/services/define-response.scm \
	-extend macros/core/services/structure.scm \
	sources/core/services/get-customer-service.scm -o \
	sources/core/services/get-customer-service.o

sources/core/services/get-customers-service.o : sources/core/services/get-customers-service.scm
	csc -c \
	-extend macros/core/services/define-request.scm \
	-extend macros/core/services/define-response.scm \
	-extend macros/core/services/structure.scm \
	sources/core/services/get-customers-service.scm -o \
	sources/core/services/get-customers-service.o

sources/core/services/new-customer-service.o : sources/core/services/new-customer-service.scm
	csc -c \
	-extend macros/core/services/define-request.scm \
	-extend macros/core/services/define-response.scm \
	-extend macros/core/services/structure.scm \
	sources/core/services/new-customer-service.scm -o \
	sources/core/services/new-customer-service.o

sources/core/services/update-customer-service.o : sources/core/services/update-customer-service.scm
	csc -c \
	-extend macros/core/services/define-request.scm \
	-extend macros/core/services/define-response.scm \
	-extend macros/core/services/structure.scm \
	sources/core/services/update-customer-service.scm -o \
	sources/core/services/update-customer-service.o

compile-core-tables : sources/core/tables/customers-table.o \
                      sources/core/tables/shipping-addresses-table.o

sources/core/tables/customers-table.o : sources/core/tables/customers-table.scm
	csc -c -extend macros/core/tables/define-table.scm \
	sources/core/tables/customers-table.scm -o \
	sources/core/tables/customers-table.o

sources/core/tables/shipping-addresses-table.o : sources/core/tables/shipping-addresses-table.scm
	csc -c -extend macros/core/tables/define-table.scm \
	sources/core/tables/shipping-addresses-table.scm -o \
	sources/core/tables/shipping-addresses-table.o

compile-foreign-interfaces : compile-foreign-interfaces-fastcgi \
                             compile-foreign-interfaces-hungarian \
                             compile-foreign-interfaces-jansson \
                             compile-foreign-interfaces-pcre \
                             compile-foreign-interfaces-scdtl \
                             compile-foreign-interfaces-sqlite

compile-foreign-interfaces-fastcgi : sources/foreign-interfaces/fastcgi.o

sources/foreign-interfaces/fastcgi.o : sources/foreign-interfaces/fastcgi.scm
	csc -c -I/usr/local/include \
	sources/foreign-interfaces/fastcgi.scm -o \
	sources/foreign-interfaces/fastcgi.o

compile-foreign-interfaces-hungarian : sources/foreign-interfaces/hungarian.o

sources/foreign-interfaces/hungarian.o : sources/foreign-interfaces/hungarian.scm
	csc -c -I/usr/local/include \
	sources/foreign-interfaces/hungarian.scm -o \
	sources/foreign-interfaces/hungarian.o

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

compile-infrastructure : compile-infrastructure-compare \
                         compile-infrastructure-date-time \
                         compile-infrastructure-debug \
                         compile-infrastructure-exceptions \
                         compile-infrastructure-hash \
                         compile-infrastructure-http \
                         compile-infrastructure-i18n \
                         compile-infrastructure-json \
                         compile-infrastructure-latex \
                         compile-infrastructure-list \
                         compile-infrastructure-math \
                         compile-infrastructure-matrix \
                         compile-infrastructure-records \
                         compile-infrastructure-regex \
                         compile-infrastructure-services \
                         compile-infrastructure-sql \
                         compile-infrastructure-test \
                         compile-infrastructure-validation \
                         compile-infrastructure-validation-service-request

compile-infrastructure-compare : sources/infrastructure/compare/compare.o

sources/infrastructure/compare/compare.o : sources/infrastructure/compare/compare.scm
	csc -c \
	sources/infrastructure/compare/compare.scm -o \
	sources/infrastructure/compare/compare.o

compile-infrastructure-date-time : sources/infrastructure/date-time/date-time.o \
                                   sources/infrastructure/date-time/date-time-intern.o

sources/infrastructure/date-time/date-time.o : sources/infrastructure/date-time/date-time.scm
	csc -c \
	sources/infrastructure/date-time/date-time.scm -o \
	sources/infrastructure/date-time/date-time.o

sources/infrastructure/date-time/date-time-intern.o : sources/infrastructure/date-time/date-time-intern.scm
	csc -c \
	sources/infrastructure/date-time/date-time-intern.scm -o \
	sources/infrastructure/date-time/date-time-intern.o

compile-infrastructure-debug : sources/infrastructure/debug/debug.o

sources/infrastructure/debug/debug.o : sources/infrastructure/debug/debug.scm
	csc -c \
	sources/infrastructure/debug/debug.scm -o \
	sources/infrastructure/debug/debug.o

compile-infrastructure-exceptions : sources/infrastructure/exceptions/exceptions.o

sources/infrastructure/exceptions/exceptions.o : sources/infrastructure/exceptions/exceptions.scm
	csc -c \
	sources/infrastructure/exceptions/exceptions.scm -o \
	sources/infrastructure/exceptions/exceptions.o

compile-infrastructure-hash : sources/infrastructure/hash/hash.o \
                              sources/infrastructure/hash/hash-intern.o

sources/infrastructure/hash/hash.o : sources/infrastructure/hash/hash.scm
	csc -c \
	sources/infrastructure/hash/hash.scm -o \
	sources/infrastructure/hash/hash.o

sources/infrastructure/hash/hash-intern.o : sources/infrastructure/hash/hash-intern.scm
	csc -c \
	sources/infrastructure/hash/hash-intern.scm -o \
	sources/infrastructure/hash/hash-intern.o

compile-infrastructure-http : sources/infrastructure/http/http.o \
                              sources/infrastructure/http/http-intern.o \
                              sources/infrastructure/http/http-toplevel.o \
                              sources/infrastructure/http/main.o

sources/infrastructure/http/http.o : sources/infrastructure/http/http.scm
	csc -c \
	sources/infrastructure/http/http.scm -o \
	sources/infrastructure/http/http.o

sources/infrastructure/http/http-intern.o : sources/infrastructure/http/http-intern.scm
	csc -c \
	sources/infrastructure/http/http-intern.scm -o \
	sources/infrastructure/http/http-intern.o

sources/infrastructure/http/http-toplevel.o : sources/infrastructure/http/http-toplevel.scm
	csc -c -e \
	sources/infrastructure/http/http-toplevel.scm -o \
	sources/infrastructure/http/http-toplevel.o

sources/infrastructure/http/main.o : sources/infrastructure/http/main.c
	csc -c \
	sources/infrastructure/http/main.c -o \
	sources/infrastructure/http/main.o

compile-infrastructure-i18n : sources/infrastructure/i18n/i18n.o

sources/infrastructure/i18n/i18n.o : sources/infrastructure/i18n/i18n.scm
	csc -c \
	sources/infrastructure/i18n/i18n.scm -o \
	sources/infrastructure/i18n/i18n.o

compile-infrastructure-json : sources/infrastructure/json/json.o \
                              sources/infrastructure/json/json-convert.o \
                              sources/infrastructure/json/json-format.o \
                              sources/infrastructure/json/json-intern.o \
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

sources/infrastructure/json/json-intern.o : sources/infrastructure/json/json-intern.scm
	csc -c \
	sources/infrastructure/json/json-intern.scm -o \
	sources/infrastructure/json/json-intern.o

sources/infrastructure/json/json-parse.o : sources/infrastructure/json/json-parse.scm
	csc -c \
	sources/infrastructure/json/json-parse.scm -o \
	sources/infrastructure/json/json-parse.o

compile-infrastructure-latex : sources/infrastructure/latex/latex.o

sources/infrastructure/latex/latex.o : sources/infrastructure/latex/latex.scm
	csc -c \
	sources/infrastructure/latex/latex.scm -o \
	sources/infrastructure/latex/latex.o

compile-infrastructure-list : sources/infrastructure/list/list.o \
                              sources/infrastructure/list/list-intern.o

sources/infrastructure/list/list.o : sources/infrastructure/list/list.scm
	csc -c \
	sources/infrastructure/list/list.scm -o \
	sources/infrastructure/list/list.o

sources/infrastructure/list/list-intern.o : sources/infrastructure/list/list-intern.scm
	csc -c \
	sources/infrastructure/list/list-intern.scm -o \
	sources/infrastructure/list/list-intern.o

compile-infrastructure-math : sources/infrastructure/math/math.o

sources/infrastructure/math/math.o : sources/infrastructure/math/math.scm
	csc -c \
	sources/infrastructure/math/math.scm -o \
	sources/infrastructure/math/math.o

compile-infrastructure-matrix : sources/infrastructure/matrix/matrix.o \
                                sources/infrastructure/matrix/matrix-intern.o

sources/infrastructure/matrix/matrix.o : sources/infrastructure/matrix/matrix.scm
	csc -c \
	sources/infrastructure/matrix/matrix.scm -o \
	sources/infrastructure/matrix/matrix.o

sources/infrastructure/matrix/matrix-intern.o : sources/infrastructure/matrix/matrix-intern.scm
	csc -c \
	sources/infrastructure/matrix/matrix-intern.scm -o \
	sources/infrastructure/matrix/matrix-intern.o

compile-infrastructure-records : sources/infrastructure/records/records.o

sources/infrastructure/records/records.o : sources/infrastructure/records/records.scm
	csc -c \
	sources/infrastructure/records/records.scm -o \
	sources/infrastructure/records/records.o

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

compile-infrastructure-test : sources/infrastructure/test/test.o \
                              sources/infrastructure/test/test-runner.o

sources/infrastructure/test/test.o : sources/infrastructure/test/test.scm
	csc -c \
	sources/infrastructure/test/test.scm -o \
	sources/infrastructure/test/test.o

sources/infrastructure/test/test-runner.o : sources/infrastructure/test/test-runner.scm
	csc -c \
	sources/infrastructure/test/test-runner.scm -o \
	sources/infrastructure/test/test-runner.o

compile-infrastructure-validation : sources/infrastructure/validation/validation.o

sources/infrastructure/validation/validation.o : sources/infrastructure/validation/validation.scm
	csc -c \
	sources/infrastructure/validation/validation.scm -o \
	sources/infrastructure/validation/validation.o

compile-infrastructure-validation-service-request : sources/infrastructure/validation/validation-service-request.o

sources/infrastructure/validation/validation-service-request.o : sources/infrastructure/validation/validation-service-request.scm
	csc -c \
	sources/infrastructure/validation/validation-service-request.scm -o \
	sources/infrastructure/validation/validation-service-request.o

compile-tests : compile-tests-services

compile-tests-services : tests/core/services/delete-customer-service-test.o \
                         tests/core/services/get-customer-service-test.o \
                         tests/core/services/get-customers-service-test.o \
                         tests/core/services/new-customer-service-test.o \
                         tests/core/services/update-customer-service-test.o

tests/core/services/delete-customer-service-test.o : tests/core/services/delete-customer-service-test.scm \
                                                     macros/infrastructure/define-test.scm
	csc -c -extend macros/infrastructure/define-test.scm \
	tests/core/services/delete-customer-service-test.scm -o \
	tests/core/services/delete-customer-service-test.o

tests/core/services/get-customer-service-test.o : tests/core/services/get-customer-service-test.scm \
                                                  macros/infrastructure/define-test.scm
	csc -c -extend macros/infrastructure/define-test.scm \
	tests/core/services/get-customer-service-test.scm -o \
	tests/core/services/get-customer-service-test.o

tests/core/services/get-customers-service-test.o : tests/core/services/get-customers-service-test.scm \
                                                   macros/infrastructure/define-test.scm
	csc -c -extend macros/infrastructure/define-test.scm \
	tests/core/services/get-customers-service-test.scm -o \
	tests/core/services/get-customers-service-test.o

tests/core/services/new-customer-service-test.o : tests/core/services/new-customer-service-test.scm \
                                                  macros/infrastructure/define-test.scm
	csc -c -extend macros/infrastructure/define-test.scm \
	tests/core/services/new-customer-service-test.scm -o \
	tests/core/services/new-customer-service-test.o

tests/core/services/update-customer-service-test.o : tests/core/services/update-customer-service-test.scm \
                                                     macros/infrastructure/define-test.scm
	csc -c -extend macros/infrastructure/define-test.scm \
	tests/core/services/update-customer-service-test.scm -o \
	tests/core/services/update-customer-service-test.o

ctags : compile

link : link-bindings \
       link-core \
       link-foreign-interfaces \
       link-infrastructure \
       link-tests

link-bindings : scheme-experimentations-bindings-http

scheme-experimentations-bindings-http : sources/core/core.o \
                                        sources/foreign-interfaces/foreign-interfaces.o \
                                        sources/infrastructure/infrastructure.o \
                                        sources/bindings/http/delete-customer-service-http-binding.o \
                                        sources/bindings/http/get-customer-service-http-binding.o \
                                        sources/bindings/http/get-customers-service-http-binding.o \
                                        sources/bindings/http/new-customer-service-http-binding.o \
                                        sources/bindings/http/update-customer-service-http-binding.o \
                                        sources/infrastructure/http/http.o \
                                        sources/infrastructure/http/http-intern.o \
                                        sources/infrastructure/http/http-toplevel.o \
                                        sources/infrastructure/http/main.o
	csc \
	-lfcgi \
	-lhungarian \
	-ljansson \
	-lpcre \
	-lsqlite3 \
	sources/core/core.o \
	sources/foreign-interfaces/foreign-interfaces.o \
	sources/infrastructure/infrastructure.o \
	sources/bindings/http/delete-customer-service-http-binding.o \
	sources/bindings/http/get-customer-service-http-binding.o \
	sources/bindings/http/get-customers-service-http-binding.o \
	sources/bindings/http/new-customer-service-http-binding.o \
	sources/bindings/http/update-customer-service-http-binding.o \
	sources/infrastructure/http/http.o \
	sources/infrastructure/http/http-intern.o \
	sources/infrastructure/http/http-toplevel.o \
	sources/infrastructure/http/main.o \
	-o scheme-experimentations-bindings-http

link-core : sources/core/core.o

sources/core/core.o : sources/core/services/delete-customer-service.o \
                      sources/core/services/get-customer-service.o \
                      sources/core/services/get-customers-service.o \
                      sources/core/services/new-customer-service.o \
                      sources/core/services/update-customer-service.o \
                      sources/core/tables/customers-table.o \
                      sources/core/tables/shipping-addresses-table.o
	ld --relocatable \
	sources/core/services/delete-customer-service.o \
	sources/core/services/get-customer-service.o \
	sources/core/services/get-customers-service.o \
	sources/core/services/new-customer-service.o \
	sources/core/services/update-customer-service.o \
	sources/core/tables/customers-table.o \
	sources/core/tables/shipping-addresses-table.o \
	-o sources/core/core.o

link-foreign-interfaces : sources/foreign-interfaces/foreign-interfaces.o

sources/foreign-interfaces/foreign-interfaces.o : sources/foreign-interfaces/fastcgi.o \
                                                  sources/foreign-interfaces/hungarian.o \
                                                  sources/foreign-interfaces/jansson.o \
                                                  sources/foreign-interfaces/pcre.o \
                                                  sources/foreign-interfaces/scdtl.o \
                                                  sources/foreign-interfaces/sqlite.o
	ld --relocatable \
	sources/foreign-interfaces/fastcgi.o \
	sources/foreign-interfaces/hungarian.o \
	sources/foreign-interfaces/jansson.o \
	sources/foreign-interfaces/pcre.o \
	sources/foreign-interfaces/scdtl.o \
	sources/foreign-interfaces/sqlite.o \
	-o sources/foreign-interfaces/foreign-interfaces.o

link-infrastructure : sources/infrastructure/infrastructure.o

sources/infrastructure/infrastructure.o : sources/infrastructure/compare/compare.o \
                                          sources/infrastructure/date-time/date-time.o \
                                          sources/infrastructure/date-time/date-time-intern.o \
                                          sources/infrastructure/debug/debug.o \
                                          sources/infrastructure/exceptions/exceptions.o \
                                          sources/infrastructure/hash/hash.o \
                                          sources/infrastructure/hash/hash-intern.o \
                                          sources/infrastructure/i18n/i18n.o \
                                          sources/infrastructure/json/json.o \
                                          sources/infrastructure/json/json-convert.o \
                                          sources/infrastructure/json/json-format.o \
                                          sources/infrastructure/json/json-intern.o \
                                          sources/infrastructure/json/json-parse.o \
                                          sources/infrastructure/latex/latex.o \
                                          sources/infrastructure/list/list.o \
                                          sources/infrastructure/list/list-intern.o \
                                          sources/infrastructure/math/math.o \
                                          sources/infrastructure/matrix/matrix.o \
                                          sources/infrastructure/matrix/matrix-intern.o \
                                          sources/infrastructure/records/records.o \
                                          sources/infrastructure/regex/regex.o \
                                          sources/infrastructure/regex/regex-intern.o \
                                          sources/infrastructure/services/services.o \
                                          sources/infrastructure/sql/sql.o \
                                          sources/infrastructure/sql/sql-convert.o \
                                          sources/infrastructure/sql/sql-intern.o \
                                          sources/infrastructure/test/test.o \
                                          sources/infrastructure/validation/validation.o \
                                          sources/infrastructure/validation/validation-service-request.o
	ld --relocatable \
	sources/infrastructure/compare/compare.o \
	sources/infrastructure/date-time/date-time.o \
	sources/infrastructure/date-time/date-time-intern.o \
	sources/infrastructure/debug/debug.o \
	sources/infrastructure/exceptions/exceptions.o \
	sources/infrastructure/hash/hash.o \
	sources/infrastructure/hash/hash-intern.o \
	sources/infrastructure/i18n/i18n.o \
	sources/infrastructure/json/json.o \
	sources/infrastructure/json/json-convert.o \
	sources/infrastructure/json/json-format.o \
	sources/infrastructure/json/json-intern.o \
	sources/infrastructure/json/json-parse.o \
	sources/infrastructure/latex/latex.o \
	sources/infrastructure/list/list.o \
	sources/infrastructure/list/list-intern.o \
	sources/infrastructure/math/math.o \
	sources/infrastructure/matrix/matrix.o \
	sources/infrastructure/matrix/matrix-intern.o \
	sources/infrastructure/records/records.o \
	sources/infrastructure/regex/regex.o \
	sources/infrastructure/regex/regex-intern.o \
	sources/infrastructure/services/services.o \
	sources/infrastructure/sql/sql.o \
	sources/infrastructure/sql/sql-convert.o \
	sources/infrastructure/sql/sql-intern.o \
	sources/infrastructure/test/test.o \
	sources/infrastructure/validation/validation.o \
	sources/infrastructure/validation/validation-service-request.o \
	-o sources/infrastructure/infrastructure.o

link-tests : scheme-experimentations-tests

scheme-experimentations-tests : sources/core/core.o \
                                sources/foreign-interfaces/foreign-interfaces.o \
                                sources/infrastructure/infrastructure.o \
                                sources/infrastructure/test/test-runner.o \
                                tests/core/services/delete-customer-service-test.o \
                                tests/core/services/get-customer-service-test.o \
                                tests/core/services/get-customers-service-test.o \
                                tests/core/services/new-customer-service-test.o \
                                tests/core/services/update-customer-service-test.o
	csc \
	-lfcgi \
	-lhungarian \
	-ljansson \
	-lpcre \
	-lsqlite3 \
	sources/core/core.o \
	sources/foreign-interfaces/foreign-interfaces.o \
	sources/infrastructure/infrastructure.o \
	sources/infrastructure/test/test-runner.o \
	tests/core/services/delete-customer-service-test.o \
	tests/core/services/get-customer-service-test.o \
	tests/core/services/get-customers-service-test.o \
	tests/core/services/new-customer-service-test.o \
	tests/core/services/update-customer-service-test.o \
	-o scheme-experimentations-tests

install :
	cp scheme-experimentations-bindings-http /usr/local/apache2/api/

tools : tools-chicken-scheme \
        tools-fastcgi \
        tools-httpd \
        tools-hungarian \
        tools-jansson \
        tools-pcre \
        tools-sqlite \

tools-chicken-scheme : tools/chicken-scheme/chicken-4.8.0.tar.gz
	mkdir /tmp/chicken-scheme
	tar -x -z -f tools/chicken-scheme/chicken-4.8.0.tar.gz -C /tmp/chicken-scheme
	$(MAKE) -C /tmp/chicken-scheme/chicken-4.8.0 PLATFORM=linux
	$(MAKE) -C /tmp/chicken-scheme/chicken-4.8.0 PLATFORM=linux install
	rm -r /tmp/chicken-scheme

tools-fastcgi : tools-httpd \
                tools-fastcgi-fcgi \
                tools-fastcgi-mod-fcgid

tools-fastcgi-fcgi : tools/fastcgi/fcgi-2.4.0.tar.gz
	mkdir /tmp/fcgi
	tar -x -z -f tools/fastcgi/fcgi-2.4.0.tar.gz -C /tmp/fcgi
	cd /tmp/fcgi/fcgi-2.4.0/libfcgi && sed '25 i #include <stdio.h>' fcgio.cpp > fcgio.cpp.tmp
	cd /tmp/fcgi/fcgi-2.4.0/libfcgi && mv fcgio.cpp.tmp fcgio.cpp
	cd /tmp/fcgi/fcgi-2.4.0 && ./configure
	$(MAKE) -C /tmp/fcgi/fcgi-2.4.0
	$(MAKE) -C /tmp/fcgi/fcgi-2.4.0 install
	rm -r /tmp/fcgi

tools-fastcgi-mod-fcgid : tools/fastcgi/mod_fcgid-2.3.7.tar.gz
	mkdir /tmp/mod_fcgid
	tar -x -z -f tools/fastcgi/mod_fcgid-2.3.7.tar.gz -C /tmp/mod_fcgid
	cd /tmp/mod_fcgid/mod_fcgid-2.3.7 && PATH=$(PATH):/usr/local/apache2/bin && ./configure.apxs
	$(MAKE) -C /tmp/mod_fcgid/mod_fcgid-2.3.7
	$(MAKE) -C /tmp/mod_fcgid/mod_fcgid-2.3.7 install
	rm -r /tmp/mod_fcgid

tools-httpd : tools-pcre \
              tools-httpd-apr \
              tools-httpd-apr-util \
              tools-httpd-httpd

tools-httpd-apr :
	mkdir /tmp/apr
	tar -x -z -f tools/httpd/apr-1.4.6.tar.gz -C /tmp/apr
	cd /tmp/apr/apr-1.4.6 && ./configure
	$(MAKE) -C /tmp/apr/apr-1.4.6
	$(MAKE) -C /tmp/apr/apr-1.4.6 install
	rm -r /tmp/apr

tools-httpd-apr-util :
	mkdir /tmp/apr-util
	tar -x -z -f tools/httpd/apr-util-1.5.1.tar.gz -C /tmp/apr-util
	cd /tmp/apr-util/apr-util-1.5.1 && ./configure --with-apr=/usr/local/apr
	$(MAKE) -C /tmp/apr-util/apr-util-1.5.1
	$(MAKE) -C /tmp/apr-util/apr-util-1.5.1 install
	rm -r /tmp/apr-util

tools-httpd-httpd :
	mkdir /tmp/httpd
	tar -x -z -f tools/httpd/httpd-2.4.3.tar.gz -C /tmp/httpd
	cd /tmp/httpd/httpd-2.4.3 && ./configure --enable-so
	$(MAKE) -C /tmp/httpd/httpd-2.4.3
	$(MAKE) -C /tmp/httpd/httpd-2.4.3 install
	rm -r /tmp/httpd

tools-hungarian :
	mkdir /tmp/hungarian
	tar -x -z -f tools/hungarian/libhungarian-v0.1.2.tgz -C /tmp/hungarian
	cd /tmp/hungarian/libhungarian && make clean
	cd /tmp/hungarian/libhungarian && make
	cd /tmp/hungarian/libhungarian && cp hungarian.h /usr/local/include
	cd /tmp/hungarian/libhungarian && cp libhungarian.a /usr/local/lib
	rm -r /tmp/hungarian

tools-jansson :
	mkdir /tmp/jansson
	tar -x -z -f tools/jansson/jansson-2.4.tar.gz -C /tmp/jansson
	cd /tmp/jansson/jansson-2.4 && ./configure
	$(MAKE) -C /tmp/jansson/jansson-2.4
	$(MAKE) -C /tmp/jansson/jansson-2.4 install
	rm -r /tmp/jansson

tools-pcre :
	mkdir /tmp/pcre
	tar -x -z -f tools/pcre/pcre-8.32.tar.gz -C /tmp/pcre
	cd /tmp/pcre/pcre-8.32 && ./configure
	$(MAKE) -C /tmp/pcre/pcre-8.32
	$(MAKE) -C /tmp/pcre/pcre-8.32 install
	rm -r /tmp/pcre

tools-sqlite :
	mkdir /tmp/sqlite
	tar -x -z -f tools/sqlite/sqlite-autoconf-3071502.tar.gz -C /tmp/sqlite
	cd /tmp/sqlite/sqlite-autoconf-3071502 && ./configure
	$(MAKE) -C /tmp/sqlite/sqlite-autoconf-3071502
	$(MAKE) -C /tmp/sqlite/sqlite-autoconf-3071502 install
	rm -r /tmp/sqlite
	ldconfig

apache-configuration :
	mkdir /usr/local/apache2/api
	echo "" >> /usr/local/apache2/conf/httpd.conf
	echo "ScriptAlias /api/ \"/usr/local/apache2/api/\"" >> /usr/local/apache2/conf/httpd.conf
	echo "" >> /usr/local/apache2/conf/httpd.conf
	echo "<Directory \"/usr/local/apache2/api\">" >> /usr/local/apache2/conf/httpd.conf
	echo "    AllowOverride None" >> /usr/local/apache2/conf/httpd.conf
	echo "    Require all granted" >> /usr/local/apache2/conf/httpd.conf
	echo "    SetHandler fcgid-script" >> /usr/local/apache2/conf/httpd.conf
	echo "    Options +ExecCGI" >> /usr/local/apache2/conf/httpd.conf
	echo "    FcgidWrapper /usr/local/apache2/api/scheme-experimentations-bindings-http virtual" >> /usr/local/apache2/conf/httpd.conf
	echo "</Directory>" >> /usr/local/apache2/conf/httpd.conf
	echo "" >> /usr/local/apache2/conf/httpd.conf

database :
	mkdir /var/databases
	sqlite3 /var/databases/customers.db < sources/database/create-database.sql
	sqlite3 /var/databases/customers.db "PRAGMA journal_mode=WAL;"
	chmod 777 /var/databases
	chmod 777 /var/databases/customers.db
