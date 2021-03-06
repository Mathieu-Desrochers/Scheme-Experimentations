
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table definition

(define-syntax define-table
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; encapsulates a column
      (define (make-column name symbol type) (list name symbol type))
      (define (column-name column) (car column))
      (define (column-symbol column) (cadr column))
      (define (column-type column) (caddr column))

      ;; makes columns based on their definition
      (define (make-columns columns-definition)
        (map
          (lambda (column-definition)
            (make-column
              (car column-definition)
              (string->symbol (car column-definition))
              (cadr column-definition)))
          columns-definition))

      ;; joins a set of columns name
      (define (join-columns-name columns)
        (string-join
          (map
            (lambda (column)
              (string-append "\"" (column-name column) "\""))
            columns)
          ", "))

      ;; joins a set of columns variable
      (define (join-columns-variable columns)
        (string-join
          (map
            (lambda (column-index)
              (string-append "?" (number->string (+ column-index 1))))
            (iota (length columns)))
          ", "))

      ;; joins a set of columns variable assignation
      (define (join-columns-variable-assignation columns first-variable-number)
        (string-join
          (map
            (lambda (column-index)
              (string-append
                "\"" (column-name (list-ref columns column-index)) "\" = "
                "?" (number->string (+ column-index first-variable-number))))
            (iota (length columns)))
          ", "))

      ;; returns a procedure that makes a row
      ;; from a result of the sql-read procedure
      (define (make-row-from-sql-read-result row-symbol columns)
        `(lambda (sql-read-result)
          (apply
            ,(symbol-append 'make- row-symbol)
            (list
              ,@(map
                (lambda (column-index)
                  `(sql-upgrade-value
                    (list-ref sql-read-result ,column-index)
                    ',(column-type (list-ref columns column-index))))
                (iota (length columns)))))))

      ;; returns downgraded row values for a set of columns
      (define (sql-downgrade-row-values row-symbol columns)
        (map
          (lambda (column)
            (let ((boolean-type? (eq? (column-type column) 'boolean)))
              `(sql-downgrade-value
                (,(symbol-append row-symbol '- (column-symbol column)) ,row-symbol)
                ,boolean-type?)))
          columns))

      ;; parses the expression
      (let* ((table-symbol (car (list-ref exp 1)))
             (table-name (cadr (list-ref exp 1)))
             (row-symbol (car (list-ref exp 2)))
             (columns (make-columns (cdr (list-ref exp 2))))
             (id-column (car columns))
             (value-columns (cdr columns))
             (custom-selects (cdr (list-ref exp 3)))
             (custom-single-value-selects (cdr (list-ref exp 4)))
             (custom-executes (cdr (list-ref exp 5))))
        `(begin

          (declare (uses sql))
          (declare (uses sql-convert))

          ;; encapsulates a row
          (define-record ,row-symbol ,@(map column-symbol columns))

          ;; inserts a row
          (define (,(symbol-append table-symbol '-insert) sql-connection ,row-symbol)
            (sql-execute
              sql-connection
              ,(string-append
                "INSERT INTO \"" table-name "\" (" (join-columns-name value-columns) ") "
                "VALUES (" (join-columns-variable value-columns) ");")
              (list
                ,@(sql-downgrade-row-values row-symbol value-columns)))
            (caar
              (sql-read
                sql-connection
                "SELECT last_insert_rowid();"
                (list))))

          ;; selects a row by id
          (define (,(symbol-append table-symbol '-select-by- (column-symbol id-column)) sql-connection ,(column-symbol id-column))
            (map ,(make-row-from-sql-read-result row-symbol columns)
              (sql-read
                sql-connection
                ,(string-append
                  "SELECT * "
                  "FROM \"" table-name "\" "
                  "WHERE \"" (column-name id-column) "\" = ?1;")
                (list
                  ,(column-symbol id-column)))))

          ;; selects all rows
          (define (,(symbol-append table-symbol '-select-all) sql-connection)
            (map ,(make-row-from-sql-read-result row-symbol columns)
              (sql-read
                sql-connection
                ,(string-append
                  "SELECT * "
                  "FROM \"" table-name "\";")
                (list))))

          ;; updates a row
          (define (,(symbol-append table-symbol '-update) sql-connection ,row-symbol)
            (sql-execute
              sql-connection
              ,(string-append
                "UPDATE \"" table-name "\" "
                "SET " (join-columns-variable-assignation value-columns 2)
                "WHERE \"" (column-name id-column) "\" = ?1;")
              (list
                ,@(sql-downgrade-row-values row-symbol columns))))

          ;; deletes a row
          (define (,(symbol-append table-symbol '-delete) sql-connection ,row-symbol)
            (sql-execute
              sql-connection
              ,(string-append
                "DELETE "
                "FROM \"" table-name "\" "
                "WHERE \"" (column-name id-column) "\" = ?1;")
              (list
                (,(symbol-append row-symbol '- (column-symbol id-column)) ,row-symbol))))

          ;; selects based on custom statements
          ,@(map
            (lambda (custom-select)
              (let ((custom-select-symbol (car custom-select))
                    (custom-select-sql (cadr custom-select))
                    (custom-select-parameters (cddr custom-select)))
                `(define (,custom-select-symbol sql-connection ,@custom-select-parameters)
                  (map ,(make-row-from-sql-read-result row-symbol columns)
                    (sql-read
                      sql-connection
                      ,custom-select-sql
                      (list
                        ,@(map
                          (lambda (custom-select-parameter)
                            `(sql-downgrade-value
                              ,custom-select-parameter
                              #t))
                          custom-select-parameters)))))))
            custom-selects)

          ;; selects for single values based on custom statements
          ,@(map
            (lambda (custom-single-value-select)
              (let ((custom-single-value-select-symbol (car custom-single-value-select))
                    (custom-single-value-select-sql (cadr custom-single-value-select))
                    (custom-single-value-select-parameters (cddr custom-single-value-select)))
                `(define (,custom-single-value-select-symbol sql-connection ,@custom-single-value-select-parameters)
                  (caar
                    (sql-read
                      sql-connection
                      ,custom-single-value-select-sql
                      (list
                        ,@(map
                          (lambda (custom-single-value-select-parameter)
                            `(sql-downgrade-value
                              ,custom-single-value-select-parameter
                              #t))
                          custom-single-value-select-parameters)))))))
            custom-single-value-selects)

          ;; executes based on custom statements
          ,@(map
            (lambda (custom-execute)
              (let ((custom-execute-symbol (car custom-execute))
                    (custom-execute-sql (cadr custom-execute))
                    (custom-execute-parameters (cddr custom-execute)))
                `(define (,custom-execute-symbol sql-connection ,@custom-execute-parameters)
                  (sql-execute
                    sql-connection
                    ,custom-execute-sql
                    (list
                      ,@custom-execute-parameters)))))
            custom-executes))))))
