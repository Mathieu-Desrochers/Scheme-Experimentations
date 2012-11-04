
Installing MinGW
----------------

- Run tools/mingw-get-inst-20120426.exe
- Select the following destination folder:
 - C:\MinGW
- Select the following options:
 - C Compiler
 - MSYS Basic System
 - MinGW Developer Toolkit
- Add the following folders to your PATH environment variable:
 - C:\MinGW\bin
 - C:\MinGW\msys\1.0\bin

Installing Chicken Scheme
-------------------------

- Uncompress tools/chicken-4.8.0.tar.gz to the following folder:
 - C:\Chicken
- Run the following commands:
 - make PLATFORM=mingw-msys PREFIX=C:/Chicken
 - make PLATFORM=mingw-msys PREFIX=C:/Chicken install
- Add the following folder to your PATH environment variable:
 - C:\Chicken\bin

Installing Sqlite3
------------------

- Open tools/sqlite/sqlite-amalgamation-3071401.zip
- Copy sqlite3.h to the following folder:
 - dependencies/headers
- Open tools/sqlite/sqlite-dll-win32-x86-3071401.zip
- Copy sqlite3.dll to the following folder:
 - dependencies/libraries
- Open tools/sqlite/sqlite-shell-win32-x86-3071401.zip
- Copy sqlite3.exe to the following folder:
 - C:\Program Files (x86)\sqlite
- Add the following folder to your PATH environment variable:
 - C:\Program Files (x86)\sqlite
