
(use srfi-1)

(declare (unit sql-intern))

(declare (uses exceptions))
(declare (uses sqlite))

;; binds a parameter of a sqlite3-stmt*
(define (sql-bind-parameter sqlite3-stmt* parameter-number parameter-value)
  (cond ((and (integer? parameter-value) (exact? parameter-value))
         (sqlite3-bind-int sqlite3-stmt* parameter-number parameter-value))
        ((number? parameter-value)
         (sqlite3-bind-double sqlite3-stmt* parameter-number parameter-value))
        ((string? parameter-value)
         (sqlite3-bind-text sqlite3-stmt* parameter-number parameter-value -1 sqlite3-transient))
        ((not parameter-value)
         (sqlite3-bind-null sqlite3-stmt* parameter-number))
        (else
          (abort
            (string-append
              "failed to bind parameter value "
              (format "~a" parameter-value))))))

;; binds the parameters of a sqlite3-stmt*
(define (sql-bind-parameters sqlite3-stmt* parameter-values)
  (map
    (lambda (parameter-index)
      (let* ((parameter-number (+ parameter-index 1))
             (parameter-value (list-ref parameter-values parameter-index))
             (sql-bind-parameter-result (sql-bind-parameter sqlite3-stmt* parameter-number parameter-value)))
        (unless (= sql-bind-parameter-result sqlite3-result-ok)
          (abort
            (string-append
              "failed to bind parameter at index "
              (number->string parameter-index))))))
    (iota (length parameter-values))))

;; invokes a procedure with a sqlite3-stmt*
(define (with-sqlite3-stmt* sqlite3* statement parameter-values procedure)
  (define (checked-malloc-sqlite3-stmt*)
    (let ((sqlite3-stmt** (malloc-sqlite3-stmt*)))
      (if (not sqlite3-stmt**)
        (abort "failed to allocate sqlite3-stmt*")
        sqlite3-stmt**)))
  (define (checked-sqlite3-prepare-v2 sqlite3-stmt**)
    (let ((sqlite3-prepare-v2-result (sqlite3-prepare-v2 sqlite3* statement -1 sqlite3-stmt** #f)))
      (if (not (= sqlite3-prepare-v2-result sqlite3-result-ok))
        (abort (string-append "failed to prepare statement " statement))
        (indirect-sqlite3-stmt** sqlite3-stmt**))))
  (with-guaranteed-release
    checked-malloc-sqlite3-stmt*
    (lambda (sqlite3-stmt**)
      (with-guaranteed-release
        (lambda () (checked-sqlite3-prepare-v2 sqlite3-stmt**))
        (lambda (sqlite3-stmt*)
            (sql-bind-parameters sqlite3-stmt* parameter-values)
            (procedure sqlite3-stmt*))
        sqlite3-finalize))
    free-sqlite3-stmt*))

;; reads a column from a sqlite3-stmt*
(define (sql-read-column sqlite3-stmt* column-index)
  (let ((column-type (sqlite3-column-type sqlite3-stmt* column-index)))
    (cond ((= column-type sqlite3-type-integer)
           (sqlite3-column-int sqlite3-stmt* column-index))
          ((= column-type sqlite3-type-float)
           (sqlite3-column-double sqlite3-stmt* column-index))
          ((= column-type sqlite3-type-text)
           (sqlite3-column-text sqlite3-stmt* column-index))
          ((= column-type sqlite3-type-null) #f)
          (else
            (abort
              (string-append
                "failed to read column type "
                (number->string column-type)))))))

;; reads all the rows from a sql-stmt*
(define (sql-read-all-rows statement sqlite3-stmt*)
  (define (sql-read-row sqlite3-stmt* columns-count)
    (map
      (lambda (column-index)
        (sql-read-column sqlite3-stmt* column-index))
      (iota columns-count)))
  (let ((columns-count (sqlite3-column-count sqlite3-stmt*)))
    (define (accumulate-rows rows)
      (let ((sqlite3-step-result (sqlite3-step sqlite3-stmt*)))
        (unless (or (= sqlite3-step-result sqlite3-result-row)
                    (= sqlite3-step-result sqlite3-result-done))
          (abort
            (string-append
              "failed to step statement "
              statement)))
        (if (= sqlite3-step-result sqlite3-result-row)
          (let ((row (sql-read-row sqlite3-stmt* columns-count)))
            (accumulate-rows (cons row rows)))
          rows)))
    (let ((rows (accumulate-rows '())))
      (reverse rows))))

;; invokes a procedure with a sql-connection
(define (with-make-sql-connection sqlite3* procedure)
  (let* ((sql-connection (make-sql-connection sqlite3*))
         (procedure-result (procedure sql-connection)))
    procedure-result))
