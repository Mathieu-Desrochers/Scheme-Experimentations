
(use srfi-1)

(declare (unit sql))

(declare (uses sql-intern))
(declare (uses sqlite))

;; encapsulates a sql connection
(define-record sql-connection sqlite3*)

;; invokes a procedure with a sql connection
(define (with-sql-connection database-name procedure)
  (define (checked-malloc-sqlite3*)
    (let ((sqlite3** (malloc-sqlite3*)))
      (if (not sqlite3**)
        (abort "failed to allocate sqlite3*")
        sqlite3**)))
  (define (checked-sqlite3-open sqlite3**)
    (let ((sqlite3-open-result (sqlite3-open database-name sqlite3**)))
      (if (not (= sqlite3-open-result sqlite3-result-ok))
        (abort (string-append "failed to open database " database-name))
        (indirect-sqlite3** sqlite3**))))
  (with-guaranteed-release
    checked-malloc-sqlite3*
    (lambda (sqlite3**)
      (with-guaranteed-release
        (lambda () (checked-sqlite3-open sqlite3**))
        (lambda (sqlite3*)
          (with-make-sql-connection sqlite3* procedure))
        sqlite3-close-v2))
    free-sqlite3*))

;; enables foreign keys enforcement
(define (sql-enable-foreign-keys sql-connection)
  (sql-execute sql-connection "PRAGMA foreign_keys = ON;"))

;; disables all synchronous disk writes
(define (sql-disable-synchronous-writes sql-connection)
  (sql-execute sql-connection "PRAGMA synchronous = OFF;"))

;; executes a procedure within a transaction
;; the transaction is automatically rollbacked if an exception occurs
(define (within-sql-transaction sql-connection procedure)
  (sql-execute sql-connection "BEGIN TRANSACTION;")
  (handle-exceptions
    exception
    (begin
      (with-exception-hiding
        (lambda ()
          (sql-execute sql-connection "ROLLBACK TRANSACTION;")))
      (abort exception))
    (procedure)
    (sql-execute sql-connection "COMMIT TRANSACTION;")))

;; explicitly begins a transaction
(define (sql-begin-transaction sql-connection)
  (sql-execute sql-connection "BEGIN TRANSACTION;"))

;; explicitly commits a transaction
(define (sql-commit-transaction sql-connection)
  (sql-execute sql-connection "COMMIT TRANSACTION;"))

;; explicitly rollbacks a transaction
(define (sql-rollback-transaction sql-connection)
  (sql-execute sql-connection "ROLLBACK TRANSACTION;"))

;; executes a sql statement
(define (sql-execute sql-connection statement . parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (let ((sqlite3-step-result (sqlite3-step sqlite3-stmt*)))
          (unless (= sqlite3-step-result sqlite3-result-done)
            (abort (string-append "failed to step statement " statement))))))))

;; executes a sql statement that returns rows
(define (sql-read sql-connection statement . parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (sql-read-all-rows statement sqlite3-stmt*)))))
