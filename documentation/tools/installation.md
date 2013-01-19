
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

Installing GCC
--------------

- Execute the following commands:
 - sudo apt-get install gcc
 - sudo apt-get install make

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

Installing Apache
-----------------

- Execute the following commands:
 - wget http://apache.parentingamerica.com//apr/apr-1.4.6.tar.gz
 - mkdir /tmp/apr
 - tar -x -z -f apr-1.4.6.tar.gz -C /tmp/apr
 - cd /tmp/apr/apr-1.4.6
 - ./configure
 - make
 - sudo make install

- Execute the following commands:
 - wget http://apache.parentingamerica.com//apr/apr-util-1.5.1.tar.gz
 - mkdir /tmp/apr-util
 - tar -x -z -f apr-util-1.5.1.tar.gz -C /tmp/apr-util
 - cd /tmp/apr-util/apr-util-1.5.1
 - ./configure --with-apr=/usr/local/apr
 - make
 - sudo make install

- Execute the following commands:
 - sudo apt-get install g++
 - wget ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.32.tar.gz
 - mkdir /tmp/pcre
 - tar -x -z -f pcre-8.32.tar.gz -C /tmp/pcre
 - cd /tmp/pcre-8.32
 - ./configure
 - make
 - sudo make install
 - sudo ldconfig

- Execute the following commands:
 - sudo apt-get install perl
 - wget http://apache.parentingamerica.com//httpd/httpd-2.4.3.tar.gz
 - mkdir /tmp/httpd
 - tar -x -z -f httpd-2.4.3.tar.gz -C /tmp/httpd
 - cd /tmp/httpd/httpd-2.4.3
 - ./configure --enable-so
 - make
 - sudo make install
 - sudo /usr/local/apache2/bin/apachectl -k start
