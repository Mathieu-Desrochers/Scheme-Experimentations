
csc -Idependencies/headers -c sources/infrastructure/sql/sqlite-ffi.scm
csc -c sources/infrastructure/sql/sql.scm
csc -c sources/infrastructure/validation/validation.scm

csc -c -extend sources/infrastructure/tables/table.scm sources/application/tables/customer-address-table.scm
csc -c -extend sources/infrastructure/tables/table.scm sources/application/tables/customer-table.scm

csc -c -extend C:/Chicken/chicken-syntax.scm sources/infrastructure/services/request.scm sources/application/services/new-customer-request.scm

csc -LC:/Github/Scheme-Experimentations/dependencies/libraries/ -lsqlite3 sources/infrastructure/sql/sqlite-ffi.o sources/infrastructure/sql/sql.o sources/infrastructure/validation/validation.o sources/application/tables/customer-address-table.o sources/application/tables/customer-table.o sources/application/services/new-customer-request.o -o build\build.exe

copy dependencies\libraries\sqlite3.dll build\
