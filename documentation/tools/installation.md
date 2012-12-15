
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
