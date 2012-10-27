
(c-declare #<<c-declare-end

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

/*
 * wrapper for the sqlite3_prepare_v2 function
 * hides the const char** parameter that can't be handled by gambit-c
 */
int sqlite3_prepare_v2_wrapper(sqlite3* db, const char* zSql, sqlite3_stmt** ppStmt)
{
  return sqlite3_prepare_v2(db, zSql, -1, ppStmt, NULL);
}

c-declare-end
)

; definitions for the sqlite3 pointers
(c-define-type c-type-sqlite3 "sqlite3")
(c-define-type c-type-sqlite3* (pointer c-type-sqlite3))
(c-define-type c-type-sqlite3** (pointer c-type-sqlite3*))

; definitions for the sqlite3_stmt pointers
(c-define-type c-type-sqlite3-stmt "sqlite3_stmt")
(c-define-type c-type-sqlite3-stmt* (pointer c-type-sqlite3-stmt))
(c-define-type c-type-sqlite3-stmt** (pointer c-type-sqlite3-stmt*))

; helpers for the sqlite3 pointers
(define malloc-sqlite3* (c-lambda () c-type-sqlite3** "malloc_sqlite3_pointer"))
(define indirect-sqlite3** (c-lambda (c-type-sqlite3**) c-type-sqlite3* "indirect_sqlite3_pointer_pointer"))
(define free-sqlite3* (c-lambda (c-type-sqlite3**) void "free_sqlite3_pointer"))

; helpers for the sqlite3_stmt pointers
(define malloc-sqlite3-stmt* (c-lambda () c-type-sqlite3-stmt** "malloc_sqlite3_stmt_pointer"))
(define indirect-sqlite3-stmt** (c-lambda (c-type-sqlite3-stmt**) c-type-sqlite3-stmt* "indirect_sqlite3_stmt_pointer_pointer"))
(define free-sqlite3-stmt* (c-lambda (c-type-sqlite3-stmt**) void "free_sqlite3_stmt_pointer"))

; opens a database connection
(define sqlite3-open (c-lambda (UTF-8-string c-type-sqlite3**) int "sqlite3_open"))

; prepares a sql statement
(define sqlite3-prepare-v2-wrapper (c-lambda (c-type-sqlite3* UTF-8-string c-type-sqlite3-stmt**) int "sqlite3_prepare_v2_wrapper"))

; executes a sql statement
(define sqlite3-step (c-lambda (c-type-sqlite3-stmt*) int "sqlite3_step"))

; returns the number of columns returned by a sql statement
(define sqlite3-column-count (c-lambda (c-type-sqlite3-stmt*) int "sqlite3_column_count"))

; returns the type of the column at the specified index
(define sqlite3-column-type (c-lambda (c-type-sqlite3-stmt* int) int "sqlite3_column_type"))

; returns the value of the column at the specified index
(define sqlite3-column-int (c-lambda (c-type-sqlite3-stmt* int) int "sqlite3_column_int"))
(define sqlite3-column-double (c-lambda (c-type-sqlite3-stmt* int) double "sqlite3_column_double"))

; finalizes a sql statement
(define sqlite3-finalize (c-lambda (c-type-sqlite3-stmt*) int "sqlite3_finalize"))

; closes a database connection
(define sqlite3-close-v2 (c-lambda (c-type-sqlite3*) int "sqlite3_close_v2"))


; open the database
(define sqlite3** (malloc-sqlite3*))
(display "sqlite3-open: ")
(display (sqlite3-open "Customers.db" sqlite3**))
(display "\n")

; prepare the sql statement
(define sqlite3-stmt** (malloc-sqlite3-stmt*))
(display "sqlite3-prepare-v2-wrapper: ")
(display (sqlite3-prepare-v2-wrapper (indirect-sqlite3** sqlite3**) "SELECT 1, 1.2, 'A', NULL;" sqlite3-stmt**))
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

; get the values of the columns
(display "sqlite3-column-int: ")
(display (sqlite3-column-int (indirect-sqlite3-stmt** sqlite3-stmt**) 0))
(display "\n")

(display "sqlite3-column-double: ")
(display (sqlite3-column-double (indirect-sqlite3-stmt** sqlite3-stmt**) 1))
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
