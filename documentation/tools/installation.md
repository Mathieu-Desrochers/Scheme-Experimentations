
Installing Git
--------------

- Execute the following commands:
 - sudo yum install git-core
 - git config --global user.name "Your Name"
 - git config --global user.email "your@name"
 - git config --global credential.helper 'cache --timeout=3600'

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
 - sudo make PLATFORM=linux PREFIX=/opt/chicken-scheme
 - sudo make PLATFORM=linux PREFIX=/opt/chicken-scheme install
- Add the following folder to your $PATH environment variable:
 - /opt/chicken-scheme/bin

Installing Chicken Scheme Eggs ??
---------------------------------

- Execute the following commands:
  - sudo /opt/chicken/bin/chicken-install regex
  - sudo /opt/chicken/bin/chicken-install utf8

Installing Sqlite
-----------------

- Execute the following commands:
 - sudo yum install sqlite
 - sudo yum install sqlite-devel

Installing Jansson
------------------

- Change directory to the root of the repository
- Execute the following commands:
 - mkdir /tmp/jansson
 - tar -x -z -f tools/jansson/jansson-2.4.tar.gz -C /tmp/jansson
 - cd /tmp/jansson/jansson-2.4
 - ./configure --prefix=/opt/jansson
 - sudo make
 - sudo make install
