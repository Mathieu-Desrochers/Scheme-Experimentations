
REM compiling infrastructure/json
csc -Idependencies/headers -c sources/infrastructure/json/jansson-ffi.scm

REM compiling infrastructure/services
csc -c sources/infrastructure/services/request-validation.scm

REM compiling infrastructure/sql
csc -Idependencies/headers -c sources/infrastructure/sql/sqlite-ffi.scm
csc -c sources/infrastructure/sql/sql.scm

REM compiling infrastructure/validation
csc -c sources/infrastructure/validation/validation.scm

REM compiling application/services
csc -c -extend sources/infrastructure/services/request.scm sources/application/services/new-customer-service.scm

REM compiling application/tables
csc -c -extend sources/infrastructure/tables/table.scm sources/application/tables/customer-addresses-table.scm
csc -c -extend sources/infrastructure/tables/table.scm sources/application/tables/customers-table.scm

REM compiling application
csc -c sources/application/main.scm

REM linking
csc -LC:/Github/Scheme-Experimentations/dependencies/libraries/ ^
  -llibjansson-4 ^
  -lsqlite3 ^
  sources/infrastructure/json/jansson-ffi.o ^
  sources/infrastructure/services/request-validation.o ^
  sources/infrastructure/sql/sql.o ^
  sources/infrastructure/sql/sqlite-ffi.o ^
  sources/infrastructure/validation/validation.o ^
  sources/application/services/new-customer-service.o ^
  sources/application/tables/customer-addresses-table.o ^
  sources/application/tables/customers-table.o ^
  sources/application/main.o ^
  -o build\build.exe

REM copying dependencies
copy dependencies\libraries\libjansson-4.dll build\
copy dependencies\libraries\sqlite3.dll build\
