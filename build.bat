
csc -Idependencies/headers -c sources/infrastructure/sql/sqlite-ffi.scm 
csc -c sources/infrastructure/sql/sql.scm 

csc -LC:/Github/Scheme-Experimentations/dependencies/libraries/ -lsqlite3 sources/infrastructure/sql/sqlite-ffi.o sources/infrastructure/sql/sql.o -o build\build.exe

copy dependencies\libraries\sqlite3.dll build\
