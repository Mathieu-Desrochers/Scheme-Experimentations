
Installing MinGW
----------------

- Run tools/mingw/mingw-get-inst-20120426.exe
- Select the following destination folder:
 - C:/MinGW
- Select the following options:
 - C Compiler
 - MSYS Basic System
 - MinGW Developer Toolkit
- Add the following folders to your PATH environment variable:
 - C:/MinGW/bin
 - C:/MinGW/msys/1.0/bin

Installing Chicken Scheme
-------------------------

- Uncompress tools/chicken-scheme/chicken-4.8.0.tar.gz to the following folder:
 - C:/Chicken
- Run the following commands:
 - make PLATFORM=mingw-msys PREFIX=C:/Chicken
 - make PLATFORM=mingw-msys PREFIX=C:/Chicken install
- Add the following folder to your PATH environment variable:
 - C:/Chicken/bin

Installing Chicken Scheme Eggs
------------------------------

- Execute the following commands:
  - chicken-install regex
  - chicken-install utf8
  - chicken-install -deploy -p dependencies/eggs regex
  - chicken-install -deploy -p dependencies/eggs utf8

Building Jansson
----------------

- Uncompress tools/jansson/jansson-2.4.tar.gz to the following folder:
 - C:/Jansson
- Run the following commands:
 - ./configure --prefix=C:/Jansson/build
 - make
 - make install
- Recover the following files:
 - build/include/jansson.h
 - build/include/jansson_config.h
 - build/bin/libjansson-4.dll
- Delete the following folder:
 - C:/Jansson
