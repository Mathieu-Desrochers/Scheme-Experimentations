
Installing Git
--------------

- Execute the following commands:
 - sudo yum install git-core
 - git config --global user.name "Your Name"
 - git config --global user.email "your@name"

Cloning the repository
----------------------

- Execute the following commands:
 - git clone https://github.com/Mathieu-Desrochers/Scheme-Experimentations.git

Installing GCC
--------------

- Execute the following commands:
 - sudo yum install gcc
 - sudo yum install make

Installing Chicken Scheme
-------------------------

- Change directory to the root of the repository
- Execute the following commands:
 - mkdir /tmp/chicken-scheme
 - tar -x -z -f tools/chicken-scheme/chicken-4.8.0.tar.gz -C /tmp/chicken-scheme
 - cd /tmp/chicken-scheme/chicken-4.8.0
 - make PLATFORM=linux
 - sudo make PLATFORM=linux install

Installing Sqlite
-----------------

- Change directory to the root of the repository
- Execute the following commands:
 - mkdir /tmp/sqlite
 - tar -x -z -f tools/sqlite/sqlite-autoconf-3071502.tar.gz -C /tmp/sqlite
 - cd /tmp/sqlite/sqlite-autoconf-3071502
 - ./configure
 - make
 - sudo make install
 - sudo ldconfig

Installing Jansson
------------------

- Change directory to the root of the repository
- Execute the following commands:
 - mkdir /tmp/jansson
 - tar -x -z -f tools/jansson/jansson-2.4.tar.gz -C /tmp/jansson
 - cd /tmp/jansson/jansson-2.4
 - ./configure
 - make
 - sudo make install
 - sudo ldconfig
