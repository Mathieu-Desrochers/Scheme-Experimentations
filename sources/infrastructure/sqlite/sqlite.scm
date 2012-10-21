
(c-declare #<<c-declare-end

#include <sqlite3.h>

/*
 * helper functions for the sqlite3 pointers
 */
sqlite3** malloc_sqlite3_pointer()
{
  sqlite3** sqlite3_pointer = malloc(sizeof(sqlite3*));
  return sqlite3_pointer;
}

sqlite3* indirect_sqlite3_pointer_pointer(sqlite3** sqlite3_pointer)
{
  return *sqlite3_pointer;
}

void free_sqlite3_pointer(sqlite3** sqlite3_pointer)
{
  free(sqlite3_pointer);
}

/*
 * helper functions for the sqlite3_stmt pointers
 */
sqlite3_stmt** malloc_sqlite3_stmt_pointer()
{
  sqlite3_stmt** sqlite3_stmt_pointer = malloc(sizeof(sqlite3_stmt*));
  return sqlite3_stmt_pointer;
}

sqlite3_stmt* indirect_sqlite3_stmt_pointer_pointer(sqlite3_stmt** sqlite3_stmt_pointer)
{
  return *sqlite3_stmt_pointer;
}

void free_sqlite3_stmt_pointer(sqlite3_stmt** sqlite3_stmt_pointer)
{
  free(sqlite3_stmt_pointer);
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
(c-define-type c-sqlite3 "sqlite3")
(c-define-type c-sqlite3* (pointer c-sqlite3))
(c-define-type c-sqlite3** (pointer c-sqlite3*))

; definitions for the sqlite3_stmt pointers
(c-define-type c-sqlite3-stmt "sqlite3_stmt")
(c-define-type c-sqlite3-stmt* (pointer c-sqlite3-stmt))
(c-define-type c-sqlite3-stmt** (pointer c-sqlite3-stmt*))

; helpers for the sqlite3 pointers
(define malloc-sqlite3* (c-lambda () c-sqlite3** "malloc_sqlite3_pointer"))
(define indirect-sqlite3** (c-lambda (c-sqlite3**) c-sqlite3* "indirect_sqlite3_pointer_pointer"))
(define free-sqlite3* (c-lambda (c-sqlite3**) void "free_sqlite3_pointer"))

; helpers for the sqlite3_stmt pointers
(define malloc-sqlite3-stmt* (c-lambda () c-sqlite3-stmt** "malloc_sqlite3_stmt_pointer"))
(define indirect-sqlite3-stmt** (c-lambda (c-sqlite3-stmt**) c-sqlite3-stmt* "indirect_sqlite3_stmt_pointer_pointer"))
(define free-sqlite3-stmt* (c-lambda (c-sqlite3-stmt**) void "free_sqlite3_stmt_pointer"))

; opens a database connection
(define sqlite-open (c-lambda (UTF-8-string c-sqlite3**) int "sqlite3_open"))

; prepares a sql statement
(define sqlite-prepare-v2-wrapper (c-lambda (c-sqlite3* UTF-8-string c-sqlite3-stmt**) int "sqlite3_prepare_v2_wrapper"))

; executes a sql statement
(define sqlite-step (c-lambda (c-sqlite3-stmt*) int "sqlite3_step"))

; returns the number of columns returned by a sql statement
(define sqlite-column-count (c-lambda (c-sqlite3-stmt*) int "sqlite3_column_count"))

; finalizes a sql statement
(define sqlite-finalize (c-lambda (c-sqlite3-stmt*) int "sqlite3_finalize"))

; closes a database connection
(define sqlite-close-v2 (c-lambda (c-sqlite3*) int "sqlite3_close_v2"))


; test start
(display "start")
(display "\n")

; open the database
(define sqlite3** (malloc-sqlite3*))
(display (sqlite-open "Customers.db" sqlite3**))
(display "\n")

; prepare the sql statement
(define sqlite3-stmt** (malloc-sqlite3-stmt*))
(display (sqlite-prepare-v2-wrapper (indirect-sqlite3** sqlite3**) "SELECT 0;" sqlite3-stmt**))
(display "\n")

; execute the sql statement
(display (sqlite-step (indirect-sqlite3-stmt** sqlite3-stmt**)))
(display "\n")

; get the number of returned columns
(define columns-count (sqlite-column-count (indirect-sqlite3-stmt** sqlite3-stmt**)))
(display columns-count)
(display "\n")

; finalize the sql statement
(display (sqlite-finalize (indirect-sqlite3-stmt** sqlite3-stmt**)))
(free-sqlite3-stmt* sqlite3-stmt**)
(display "\n")

; close the database
(display (sqlite-close-v2 (indirect-sqlite3** sqlite3**)))
(free-sqlite3* sqlite3**)
(display "\n")

; test end
(display "end")
(display "\n")
