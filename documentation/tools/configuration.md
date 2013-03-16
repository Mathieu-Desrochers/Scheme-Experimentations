
Configuring SQLite databases
----------------------------

- Open the database with the sqlite3 command-line utility
- Execute the following command:

            PRAGMA journal_mode=WAL;

Configuring Apache HTTP Server
------------------------------

- Open /usr/local/apache2/conf/httpd.conf

- Append the following directive:

            ScriptAlias /fcgi-bin/ "/usr/local/apache2/fcgi-bin/"

- Append the following section:

            <Directory "/usr/local/apache2/fcgi-bin">  
                AllowOverride None  
                Require all granted  
                SetHandler fcgid-script  
                Options +ExecCGI  
                FcgidWrapper /usr/local/apache2/fcgi-bin/scheme virtual  
            </Directory>
