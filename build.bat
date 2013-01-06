
REM compiling infrastructure/json
csc -Idependencies/headers -c sources/infrastructure/json/jansson-ffi.scm
csc -c sources/infrastructure/json/json.scm
csc -c sources/infrastructure/json/json-format.scm
csc -c sources/infrastructure/json/json-parse.scm

REM compiling infrastructure/sql
csc -Idependencies/headers -c sources/infrastructure/sql/sqlite-ffi.scm
csc -c sources/infrastructure/sql/sql-intern.scm
csc -c sources/infrastructure/sql/sql.scm

REM compiling infrastructure/validation
csc -c sources/infrastructure/validation/validation.scm

REM compiling application/services
csc -c -extend sources/infrastructure/services/define-request.scm ^
  -extend sources/infrastructure/services/define-response.scm ^
  sources/application/services/new-customer-service.scm

REM compiling application/tables
csc -c -extend sources/infrastructure/tables/define-table.scm ^
  sources/application/tables/customer-addresses-table.scm
csc -c -extend sources/infrastructure/tables/define-table.scm ^
  sources/application/tables/customers-table.scm

REM compiling application
csc -c sources/application/main.scm

REM linking
csc -LC:/Github/Scheme-Experimentations/dependencies/libraries/ ^
  -llibjansson-4 ^
  -lsqlite3 ^
  sources/infrastructure/json/jansson-ffi.o ^
  sources/infrastructure/json/json.o ^
  sources/infrastructure/json/json-format.o ^
  sources/infrastructure/json/json-parse.o ^
  sources/infrastructure/sql/sqlite-ffi.o ^
  sources/infrastructure/sql/sql-intern.o ^
  sources/infrastructure/sql/sql.o ^
  sources/infrastructure/validation/validation.o ^
  sources/application/services/new-customer-service.o ^
  sources/application/tables/customer-addresses-table.o ^
  sources/application/tables/customers-table.o ^
  sources/application/main.o ^
  -o build.exe ^
  -deploy

REM copying dependencies
copy dependencies\eggs\*.* build\
copy dependencies\libraries\*.* build\
