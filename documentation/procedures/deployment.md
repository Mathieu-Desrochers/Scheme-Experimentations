
Installing the compilers
------------------------

- Execute the following commands:
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

- Open /usr/local/apache2/conf/httpd.conf
- Append the following directive:

            ScriptAlias /api/ "/usr/local/apache2/api/"

- Append the following section:

            <Directory "/usr/local/apache2/api">  
                AllowOverride None  
                Require all granted  
                SetHandler fcgid-script  
                Options +ExecCGI  
                FcgidWrapper /usr/local/apache2/api/scheme virtual  
            </Directory>

Configuring the SQLite database
-------------------------------

- Open the database with the sqlite3 command-line utility
- Execute the following command:
 - PRAGMA journal_mode=WAL;

Making the application
----------------------

- Change directory to the root of the repository
- Execute the following commands:
 - make
 - sudo make install
