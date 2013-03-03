
(use srfi-1)

(declare (unit sql))

(declare (uses sql-intern))
(declare (uses sqlite))

;; encapsulates a sql connection
(define-record sql-connection sqlite3*)

;; invokes a procedure with a sql connection
(define (with-sql-connection database-name procedure)
  (let ((sqlite3** (malloc-sqlite3*)))
    (when (not sqlite3**)
      (abort "could not allocate sqlite3*"))
    (handle-exceptions exception
      (begin
        (free-sqlite3* sqlite3**)
        (abort exception))
      (let ((sqlite3-open-result (sqlite3-open database-name sqlite3**)))
        (unless (= sqlite3-open-result sqlite3-result-ok)
          (abort (string-append "could not open database " database-name)))
        (handle-exceptions exception
          (begin
            (sqlite3-close-v2 (indirect-sqlite3** sqlite3**))
            (abort exception))
          (let* ((sql-connection (make-sql-connection (indirect-sqlite3** sqlite3**)))
                 (procedure-result (procedure sql-connection)))
            (sqlite3-close-v2 (indirect-sqlite3** sqlite3**))
            (free-sqlite3* sqlite3**)
            procedure-result))))))

;; executes a sql statement
(define (sql-execute sql-connection statement . parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (let ((sqlite3-step-result (sqlite3-step sqlite3-stmt*)))
          (unless (= sqlite3-step-result sqlite3-result-done)
            (abort (string-append "could not step statement " statement))))))))

;; executes a sql statement that returns rows
(define (sql-read sql-connection statement . parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (sql-read-all-rows statement sqlite3-stmt*)))))
