
with-sql-connection
-------------------
Invokes a procedure with a sql connection to a database.

__database-name__  
The path to a Sqlite3 database file.  
Throws an exception if the file cannot be opened.

    "/var/database.db"

__procedure__

    (lambda (sql-connection) ...)

sql-enable-foreign-keys
-----------------------
Enables the foreign key enforcement on a sql connection.

__sql-connection__  
The sql connection obtained from with-sql-connection.

sql-disable-synchronous-writes
------------------------------
Disables the synchronous disk writes on a sql connection.

__sql-connection__  
The sql connection obtained from with-sql-connection.

within-sql-transaction
----------------------
Executes a procedure within a transaction.  
The transaction will be automatically committed if no exception is raised.  
Otherwise it will be automatically rollbacked.

__sql-connection__  
The sql connection obtained from with-sql-connection.

__procedure__

    (lambda () ...)

sql-execute
-----------
Executes a sql statement.
Throws an exception if the statement execution failed.

__sql-connection__  
The sql connection obtained from with-sql-connection.

__statement__  
A sql statement possibly containing parameters.

    "UPDATE customers SET rebate = ?1 WHERE age > ?2;"

__parameter-values__  
The list of values for the statement parameters.

    (list "10%" 65)

sql-read
--------
Executes a sql statement that return rows.
Throws an exception if the statement execution failed.

__sql-connection__  
The sql connection obtained from with-sql-connection.

__statement__  
A sql statement possibly containing parameters.

    "SELECT * FROM customers WHERE city = ?1;"

__parameter-values__  
The list of values for the statement parameters.

    (list "Miami")

__result__  
Each returned row is represented by a list.

    (list
      (1002 "Alice" "Miami")
      (1005 "Bob" "Miami")
      (1009 "Carl" "Miami"))
