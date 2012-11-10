
(use srfi-1)

(declare (uses sqlite-ffi))
(declare (uses sql-helpers))

; encapsulates a sql connection
(define-record sql-connection sqlite3*)

; invokes a procedure with a sql connection
(define (with-sql-connection database-name procedure)
  (let ((sqlite3** (malloc-sqlite3*)))
    (sqlite3-open database-name sqlite3**)
    (let* ((sqlite3* (indirect-sqlite3** sqlite3**))
           (sql-connection (make-sql-connection sqlite3*))
           (procedure-result (procedure sql-connection)))
        (sqlite3-close-v2 sqlite3*)
        (free-sqlite3* sqlite3**)
        procedure-result)))

; executes a sql statement
(define (sql-execute sql-connection statement . parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (sqlite3-step sqlite3-stmt*)))))

; executes a sql statement that return rows
(define (sql-read sql-connection statement . parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (sql-read-all-rows sqlite3-stmt*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display
  (with-sql-connection "customers.db"
    (lambda (sql-connection)
      (sql-execute sql-connection
        "CREATE TABLE IF NOT EXISTS Customers (CustomerID INT, Age REAL, Name TEXT, Address TEXT);")
      (sql-execute sql-connection
        "INSERT INTO Customers VALUES (?1, ?2, ?3, ?4);"
        1001 33.5 "Mathieu" "Laval")
      (sql-read sql-connection
        "SELECT * FROM Customers;"))))
