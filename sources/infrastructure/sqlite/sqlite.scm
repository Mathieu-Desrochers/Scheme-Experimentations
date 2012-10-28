
(use srfi-1)

(foreign-declare "

#include <sqlite3.h>

/*
 * helper functions for the sqlite3 pointers
 */
sqlite3** malloc_sqlite3_pointer()
{
  sqlite3** sqlite3_pointer = sqlite3_malloc(sizeof(sqlite3*));
  return sqlite3_pointer;
}

sqlite3* indirect_sqlite3_pointer_pointer(sqlite3** sqlite3_pointer)
{
  return *sqlite3_pointer;
}

void free_sqlite3_pointer(sqlite3** sqlite3_pointer)
{
  sqlite3_free(sqlite3_pointer);
}

/*
 * helper functions for the sqlite3_stmt pointers
 */
sqlite3_stmt** malloc_sqlite3_stmt_pointer()
{
  sqlite3_stmt** sqlite3_stmt_pointer = sqlite3_malloc(sizeof(sqlite3_stmt*));
  return sqlite3_stmt_pointer;
}

sqlite3_stmt* indirect_sqlite3_stmt_pointer_pointer(sqlite3_stmt** sqlite3_stmt_pointer)
{
  return *sqlite3_stmt_pointer;
}

void free_sqlite3_stmt_pointer(sqlite3_stmt** sqlite3_stmt_pointer)
{
  sqlite3_free(sqlite3_stmt_pointer);
}

")

; definitions for the sqlite3 pointers
(define-foreign-type sqlite3 "sqlite3")
(define-foreign-type sqlite3* (c-pointer sqlite3))
(define-foreign-type sqlite3** (c-pointer sqlite3*))

; definitions for the sqlite3_stmt pointers
(define-foreign-type sqlite3-stmt "sqlite3_stmt")
(define-foreign-type sqlite3-stmt* (c-pointer sqlite3-stmt))
(define-foreign-type sqlite3-stmt** (c-pointer sqlite3-stmt*))

; helpers for the sqlite3 pointers
(define malloc-sqlite3* (foreign-lambda sqlite3** "malloc_sqlite3_pointer"))
(define indirect-sqlite3** (foreign-lambda sqlite3* "indirect_sqlite3_pointer_pointer" sqlite3**))
(define free-sqlite3* (foreign-lambda void "free_sqlite3_pointer" sqlite3**))

; helpers for the sqlite3_stmt pointers
(define malloc-sqlite3-stmt* (foreign-lambda sqlite3-stmt** "malloc_sqlite3_stmt_pointer"))
(define indirect-sqlite3-stmt** (foreign-lambda sqlite3-stmt* "indirect_sqlite3_stmt_pointer_pointer" sqlite3-stmt**))
(define free-sqlite3-stmt* (foreign-lambda void "free_sqlite3_stmt_pointer" sqlite3-stmt**))

; opens a database connection
(define sqlite3-open (foreign-lambda int "sqlite3_open" (const c-string) sqlite3**))

; prepares a sql statement
(define sqlite3-prepare-v2 (foreign-lambda int "sqlite3_prepare_v2" sqlite3* (const c-string) int sqlite3-stmt** (const (c-pointer c-string))))

; executes a sql statement
(define sqlite3-step (foreign-lambda int "sqlite3_step" sqlite3-stmt*))

; returns the number of columns returned by a sql statement
(define sqlite3-column-count (foreign-lambda int "sqlite3_column_count" sqlite3-stmt*))

; returns the type of the column at the specified index
(define sqlite3-column-type (foreign-lambda int "sqlite3_column_type" sqlite3-stmt* int))

; returns the value of the column at the specified index
(define sqlite3-column-int (foreign-lambda int "sqlite3_column_int" sqlite3-stmt* int))
(define sqlite3-column-double (foreign-lambda double "sqlite3_column_double" sqlite3-stmt* int))
(define sqlite3-column-text (foreign-lambda unsigned-c-string "sqlite3_column_text" sqlite3-stmt* int))

; finalizes a sql statement
(define sqlite3-finalize (foreign-lambda int "sqlite3_finalize" sqlite3-stmt*))

; closes a database connection
(define sqlite3-close-v2 (foreign-lambda int "sqlite3_close_v2" sqlite3*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; open the database
(define sqlite3** (malloc-sqlite3*))
(display "sqlite3-open: ")
(display (sqlite3-open "customers.db" sqlite3**))
(display "\n")

; prepare the sql statement
(define sqlite3-stmt** (malloc-sqlite3-stmt*))
(display "sqlite3-prepare-v2: ")
(display (sqlite3-prepare-v2 (indirect-sqlite3** sqlite3**) "SELECT 1, 1.2, 'A', NULL;" -1 sqlite3-stmt** #f))
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
(display "\n")

; close the database
(display "sqlite3-close-v2: ")
(display (sqlite3-close-v2 (indirect-sqlite3** sqlite3**)))
(free-sqlite3* sqlite3**)
(display "\n")
