
Installing the compilers
------------------------

- Execute the following commands:
 - sudo apt-get update
 - sudo apt-get install make
 - sudo apt-get install gcc
 - sudo apt-get install g++

Cloning the repository
----------------------

- Execute the following commands:
 - sudo apt-get install git-core
 - git clone https://github.com/Mathieu-Desrochers/Scheme-Experimentations.git

Making the tools
----------------

- Change directory to the root of the repository
- Execute the following commands:
 - sudo make tools

Configuring Apache HTTP Server
------------------------------

- Change directory to the root of the repository
- Execute the following commands:
 - sudo make apache-configuration

Making the database
-------------------

- Change directory to the root of the repository
- Execute the following commands:
 - sudo make database

Making the application
----------------------

- Change directory to the root of the repository
- Execute the following commands:
 - make
 - sudo make install
