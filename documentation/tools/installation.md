
Installing Git
--------------

- Execute the following commands:
 - sudo apt-get install git-core
 - git config --global user.name "Your Name"
 - git config --global user.email "your@name"

Cloning the repository
----------------------

- Execute the following commands:
 - git clone https://github.com/Mathieu-Desrochers/Scheme-Experimentations.git

Installing Chicken Scheme
-------------------------

- Change directory to the root of the repository
- Execute the following commands:
 - mkdir /tmp/chicken-scheme
 - tar -x -z -f tools/chicken-scheme/chicken-4.8.0.tar.gz -C /tmp/chicken-scheme
 - cd /tmp/chicken-scheme/chicken-4.8.0
 - make PLATFORM=linux
 - sudo make PLATFORM=linux install

Installing SQLite
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

Installing Apache HTTP Server
-----------------------------

Prerequisite: Apache Portable Runtime

- Change directory to the root of the repository
- Execute the following commands:
 - mkdir /tmp/apr
 - tar -x -z -f tools/httpd/apr-1.4.6.tar.gz -C /tmp/apr
 - cd /tmp/apr/apr-1.4.6
 - ./configure
 - make
 - sudo make install

Prerequisite: Apache Portable Runtime Utility

- Change directory to the root of the repository
- Execute the following commands:
 - mkdir /tmp/apr-util
 - tar -x -z -f tools/httpd/apr-util-1.5.1.tar.gz -C /tmp/apr-util
 - cd /tmp/apr-util/apr-util-1.5.1
 - ./configure --with-apr=/usr/local/apr
 - make
 - sudo make install

Prerequisite: Perl-Compatible Regular Expressions Library (PCRE)

- Change directory to the root of the repository
- Execute the following commands:
 - sudo apt-get install g++
 - mkdir /tmp/pcre
 - tar -x -z -f tools/httpd/pcre-8.32.tar.gz -C /tmp/pcre
 - cd /tmp/pcre/pcre-8.32
 - ./configure
 - make
 - sudo make install
 - sudo ldconfig

Apache HTTP Server:

- Change directory to the root of the repository
- Execute the following commands:
 - mkdir /tmp/httpd
 - tar -x -z -f tools/httpd/httpd-2.4.3.tar.gz -C /tmp/httpd
 - cd /tmp/httpd/httpd-2.4.3
 - ./configure --enable-so
 - make
 - sudo make install

Installing Apache Module mod_fcgid
----------------------------------

- Change directory to the root of the repository
- Execute the following commands:
 - mkdir /tmp/mod_fcgid
 - tar -x -z -f tools/fastcgi/mod_fcgid-2.3.7.tar.gz -C /tmp/mod_fcgid
 - cd /tmp/mod_fcgid/mod_fcgid-2.3.7
 - export PATH=$PATH:/usr/local/apache2/bin
 - ./configure.apxs
 - make
 - sudo make install

Installing FastCGI Development Kit
----------------------------------

- Change directory to the root of the repository
- Execute the following commands:
 - mkdir /tmp/fcgi
 - tar -x -z -f tools/fastcgi/fcgi-2.4.0.tar.gz -C /tmp/fcgi
 - cd /tmp/fcgi/fcgi-2.4.0
 - nano libfcgi/fcgio.cpp
 - Add the following include: <stdio.h>
 - ./configure
 - make
 - sudo make install
