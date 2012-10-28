
csc -Idependencies/headers -c sources/infrastructure/sqlite/sqlite.scm 

csc -LC:/Github/Scheme-Experimentations/dependencies/libraries/ -lsqlite3 sources/infrastructure/sqlite/sqlite.o -o build\build.exe

copy dependencies\libraries\sqlite3.dll build\
