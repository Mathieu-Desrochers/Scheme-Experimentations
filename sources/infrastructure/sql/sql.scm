
(use srfi-1)

(declare (unit sql))
(declare (uses sqlite-ffi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public bindings

;; encapsulates a sql connection
(define-record sql-connection sqlite3*)

;; invokes a procedure with a sql connection
(define (with-sql-connection database-name procedure)
  (let ((sqlite3** (malloc-sqlite3*)))
    (sqlite3-open database-name sqlite3**)
    (let* ((sqlite3* (indirect-sqlite3** sqlite3**))
           (sql-connection (make-sql-connection sqlite3*))
           (procedure-result (procedure sql-connection)))
        (sqlite3-close-v2 sqlite3*)
        (free-sqlite3* sqlite3**)
        procedure-result)))

;; executes a sql statement
(define (sql-execute sql-connection statement . parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (sqlite3-step sqlite3-stmt*)))))

;; executes a sql statement that returns rows
(define (sql-read sql-connection statement . parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (sql-read-all-rows sqlite3-stmt*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private bindings

;; binds the parameters of a sqlite3-stmt*
(define (sql-bind-parameters sqlite3-stmt* parameter-values)
  (map
    (lambda (parameter-index)
      (let ((parameter-number (+ parameter-index 1))
            (parameter-value (list-ref parameter-values parameter-index)))
        (cond ((integer? parameter-value)
               (sqlite3-bind-int sqlite3-stmt* parameter-number parameter-value))
              ((number? parameter-value)
               (sqlite3-bind-double sqlite3-stmt* parameter-number parameter-value))
              ((string? parameter-value)
               (sqlite3-bind-text sqlite3-stmt* parameter-number parameter-value -1 sqlite3-transient))
              ((not parameter-value)
               (sqlite3-bind-null sqlite3-stmt* parameter-number)))))
    (iota (length parameter-values))))

;; invokes a procedure with a sqlite3-stmt*
(define (with-sqlite3-stmt* sqlite3* statement parameter-values procedure)
  (let ((sqlite3-stmt** (malloc-sqlite3-stmt*)))
    (sqlite3-prepare-v2 sqlite3* statement -1 sqlite3-stmt** #f)
    (let ((sqlite3-stmt* (indirect-sqlite3-stmt** sqlite3-stmt**)))
      (sql-bind-parameters sqlite3-stmt* parameter-values)
      (let ((procedure-result (procedure sqlite3-stmt*)))
        (sqlite3-finalize sqlite3-stmt*)
        (free-sqlite3-stmt* sqlite3-stmt**)
        procedure-result))))

;; reads a column from a sqlite3-stmt*
(define (sql-read-column sqlite3-stmt* column-index)
  (let ((column-type (sqlite3-column-type sqlite3-stmt* column-index)))
    (cond ((= column-type sqlite3-type-integer)
           (sqlite3-column-int sqlite3-stmt* column-index))
          ((= column-type sqlite3-type-float)
           (sqlite3-column-double sqlite3-stmt* column-index))
          ((= column-type sqlite3-type-text)
           (sqlite3-column-text sqlite3-stmt* column-index))
          ((= column-type sqlite3-type-null) #f))))

;; reads a row from a sqlite3-stmt*
(define (sql-read-row sqlite3-stmt* columns-count)
  (map
    (lambda (column-index)
      (sql-read-column sqlite3-stmt* column-index))
    (iota columns-count)))

;; reads all the rows from a sql-stmt*
(define (sql-read-all-rows sqlite3-stmt*)
  (let ((columns-count (sqlite3-column-count sqlite3-stmt*)))
    (define (accumulate-rows rows)
      (let ((step-result (sqlite3-step sqlite3-stmt*)))
        (if (= step-result sqlite3-result-row)
          (let ((row (sql-read-row sqlite3-stmt* columns-count)))
            (accumulate-rows (cons row rows)))
          rows)))
    (let ((rows (accumulate-rows '())))
      (reverse rows))))
