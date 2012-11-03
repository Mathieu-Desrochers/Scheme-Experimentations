
(foreign-declare "

#include <sqlite3.h>

// allocates a sqlite3* on the heap
sqlite3** malloc_sqlite3_pointer()
{
  sqlite3** sqlite3_pointer = sqlite3_malloc(sizeof(sqlite3*));
  return sqlite3_pointer;
}

// indirects the specified sqlite3**
sqlite3* indirect_sqlite3_pointer_pointer(sqlite3** sqlite3_pointer)
{
  return *sqlite3_pointer;
}

// frees the specified sqlite3*
void free_sqlite3_pointer(sqlite3** sqlite3_pointer)
{
  sqlite3_free(sqlite3_pointer);
}

// allocates a sqlite3_stmt* on the heap
sqlite3_stmt** malloc_sqlite3_stmt_pointer()
{
  sqlite3_stmt** sqlite3_stmt_pointer = sqlite3_malloc(sizeof(sqlite3_stmt*));
  return sqlite3_stmt_pointer;
}

// indirects the specified sqlite3_stmt**
sqlite3_stmt* indirect_sqlite3_stmt_pointer_pointer(sqlite3_stmt** sqlite3_stmt_pointer)
{
  return *sqlite3_stmt_pointer;
}

// frees the specified sqlite3_stmt*
void free_sqlite3_stmt_pointer(sqlite3_stmt** sqlite3_stmt_pointer)
{
  sqlite3_free(sqlite3_stmt_pointer);
}

")

; sqlite3 pointers definitions
(define-foreign-type sqlite3 "sqlite3")
(define-foreign-type sqlite3* (c-pointer sqlite3))
(define-foreign-type sqlite3** (c-pointer sqlite3*))

; sqlite3-stmt pointers definitions
(define-foreign-type sqlite3-stmt "sqlite3_stmt")
(define-foreign-type sqlite3-stmt* (c-pointer sqlite3-stmt))
(define-foreign-type sqlite3-stmt** (c-pointer sqlite3-stmt*))

; sqlite3 pointers memory management
(define malloc-sqlite3* (foreign-lambda sqlite3** "malloc_sqlite3_pointer"))
(define indirect-sqlite3** (foreign-lambda sqlite3* "indirect_sqlite3_pointer_pointer" sqlite3**))
(define free-sqlite3* (foreign-lambda void "free_sqlite3_pointer" sqlite3**))

; sqlite3-stmt pointers memory management
(define malloc-sqlite3-stmt* (foreign-lambda sqlite3-stmt** "malloc_sqlite3_stmt_pointer"))
(define indirect-sqlite3-stmt** (foreign-lambda sqlite3-stmt* "indirect_sqlite3_stmt_pointer_pointer" sqlite3-stmt**))
(define free-sqlite3-stmt* (foreign-lambda void "free_sqlite3_stmt_pointer" sqlite3-stmt**))

; special destructor behaviors
(define sqlite3-static (foreign-value "SQLITE_STATIC" (function void (c-pointer))))
(define sqlite3-transient (foreign-value "SQLITE_TRANSIENT" (function void (c-pointer))))

; opens a new database connection
(define sqlite3-open (foreign-lambda int "sqlite3_open" (const c-string) sqlite3**))

; compiles a sql statement
(define sqlite3-prepare-v2 (foreign-lambda int "sqlite3_prepare_v2" sqlite3* (const c-string) int sqlite3-stmt** (const (c-pointer c-string))))

; evaluates a sql statement
(define sqlite3-step (foreign-lambda int "sqlite3_step" sqlite3-stmt*))

; number of columns in a result set
(define sqlite3-column-count (foreign-lambda int "sqlite3_column_count" sqlite3-stmt*))

; result values from a query
(define sqlite3-column-type (foreign-lambda int "sqlite3_column_type" sqlite3-stmt* int))
(define sqlite3-column-int (foreign-lambda int "sqlite3_column_int" sqlite3-stmt* int))
(define sqlite3-column-double (foreign-lambda double "sqlite3_column_double" sqlite3-stmt* int))
(define sqlite3-column-text (foreign-lambda unsigned-c-string "sqlite3_column_text" sqlite3-stmt* int))

; binding values to prepared statements
(define sqlite3-bind-int (foreign-lambda int "sqlite3_bind_int" sqlite3-stmt* int int))
(define sqlite3-bind-double (foreign-lambda int "sqlite3_bind_double" sqlite3-stmt* int double))
(define sqlite3-bind-text (foreign-lambda int "sqlite3_bind_text" sqlite3-stmt* int (const c-string) int (function void (c-pointer))))
(define sqlite3-bind-null (foreign-lambda int "sqlite3_bind_null" sqlite3-stmt* int))

; destroy a prepared statement object
(define sqlite3-finalize (foreign-lambda int "sqlite3_finalize" sqlite3-stmt*))

; closing a database connection
(define sqlite3-close-v2 (foreign-lambda int "sqlite3_close_v2" sqlite3*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use srfi-1)

; open the database
(let ((sqlite3** (malloc-sqlite3*)))

  (display "sqlite3-open: ")
  (display (sqlite3-open "customers.db" sqlite3**))
  (display "\n")

  ; create the customers table
  (let ((sqlite3-stmt** (malloc-sqlite3-stmt*)))

    ; prepare the sql statement
    (display "sqlite3-prepare-v2: ")
    (display (sqlite3-prepare-v2 (indirect-sqlite3** sqlite3**) "CREATE TABLE IF NOT EXISTS Customers (CustomerID INT, Age REAL, Name TEXT);" -1 sqlite3-stmt** #f))
    (display "\n")

    ; execute the sql statement
    (display "sqlite3-step: ")
    (display (sqlite3-step (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (display "\n")

    ; finalize the sql statement
    (display "sqlite3-finalize: ")
    (display (sqlite3-finalize (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (free-sqlite3-stmt* sqlite3-stmt**)
    (display "\n"))

  ; insert a customer row
  (let ((sqlite3-stmt** (malloc-sqlite3-stmt*)))

    ; prepare the sql statement
    (display "sqlite3-prepare-v2: ")
    (display (sqlite3-prepare-v2 (indirect-sqlite3** sqlite3**) "INSERT INTO Customers VALUES (?1, ?2, ?3);" -1 sqlite3-stmt** #f))
    (display "\n")

    ; bind the sql parameters
    (sqlite3-bind-int (indirect-sqlite3-stmt** sqlite3-stmt**) 1 1)
    (sqlite3-bind-double (indirect-sqlite3-stmt** sqlite3-stmt**) 2 1.2)
    (sqlite3-bind-text (indirect-sqlite3-stmt** sqlite3-stmt**) 3 "A" -1 sqlite3-transient)

    ; execute the sql statement
    (display "sqlite3-step: ")
    (display (sqlite3-step (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (display "\n")

    ; finalize the sql statement
    (display "sqlite3-finalize: ")
    (display (sqlite3-finalize (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (free-sqlite3-stmt* sqlite3-stmt**)
    (display "\n"))

  ; select the customer row
  (let ((sqlite3-stmt** (malloc-sqlite3-stmt*)))

    ; prepare the sql statement
    (display "sqlite3-prepare-v2: ")
    (display (sqlite3-prepare-v2 (indirect-sqlite3** sqlite3**) "SELECT * FROM Customers;" -1 sqlite3-stmt** #f))
    (display "\n")

    ; execute the sql statement
    (display "sqlite3-step: ")
    (display (sqlite3-step (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (display "\n")

    ; get the number of returned columns
    (display "sqlite3-column-count: ")
    (define columns-count (sqlite3-column-count (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (display columns-count)
    (display "\n")

    ; get the types of the columns
    (map
      (lambda (column-index)
        (display "sqlite3-column-type: ")
        (display (sqlite3-column-type (indirect-sqlite3-stmt** sqlite3-stmt**) column-index))
        (display "\n"))
      (iota columns-count))

    ; get the value of the integer column
    (display "sqlite3-column-int: ")
    (display (sqlite3-column-int (indirect-sqlite3-stmt** sqlite3-stmt**) 0))
    (display "\n")

    ; get the value of the double column
    (display "sqlite3-column-double: ")
    (display (sqlite3-column-double (indirect-sqlite3-stmt** sqlite3-stmt**) 1))
    (display "\n")

    ; get the value of the text column
    (display "sqlite3-column-text: ")
    (display (sqlite3-column-text (indirect-sqlite3-stmt** sqlite3-stmt**) 2))
    (display "\n")

    ; finalize the sql statement
    (display "sqlite3-finalize: ")
    (display (sqlite3-finalize (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (free-sqlite3-stmt* sqlite3-stmt**)
    (display "\n"))

  ; close the database
  (display "sqlite3-close-v2: ")
  (display (sqlite3-close-v2 (indirect-sqlite3** sqlite3**)))
  (free-sqlite3* sqlite3**)
  (display "\n"))
