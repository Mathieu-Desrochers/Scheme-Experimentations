
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
      (define (join-columns-variable-assignation columns)
        (string-join
          (map
            (lambda (column-index)
              (string-append
                "\"" (column-name (list-ref columns column-index)) "\" = "
                "?" (number->string (+ column-index 1))))
            (iota (length columns)))
          ", "))

      ;; upgrades row values based on their columns type
      (define (sql-upgrade-row row columns)
        (map
          (lambda (column-index)
            (sql-upgrade-value
              (list-ref row column-index)
              (column-type (list-ref columns column-index))))
          (iota (length columns))))

      ;; parses the expression
      (let* ((table-symbol (car (list-ref exp 1)))
             (table-name (cadr (list-ref exp 1)))
             (row-symbol (car (list-ref exp 2)))
             (columns (make-columns (cdr (list-ref exp 2))))
             (id-column (car columns))
             (value-columns (cdr columns)))
        `(begin

          (declare (uses sql))
          (declare (uses sql-convert))

          ;; encapsulates a row
          (define-record ,row-symbol ,@(map column-symbol columns))

          ;; inserts a row
          (define (,(symbol-append table-symbol '-insert) sql-connection ,row-symbol)
            (sql-execute sql-connection
              (string-append
                "INSERT INTO \"" ,table-name "\" (" ,(join-columns-name value-columns) ") "
                "VALUES (" ,(join-columns-variable value-columns) ");")
              ,@(map
                (lambda (value-column)
                  `(sql-downgrade-value (,(symbol-append row-symbol '- (column-symbol value-column)) ,row-symbol)))
                value-columns))
            (caar
              (sql-read sql-connection
                "SELECT last_insert_rowid();")))

          ;; selects a row by id
          (define (,(symbol-append table-symbol '-select-by- (column-symbol id-column)) sql-connection ,(column-symbol id-column))
            (map
              (lambda (row)
                (apply ,(symbol-append 'make- row-symbol) (sql-upgrade-row row columns)))
              (sql-read sql-connection
                ,(string-append
                  "SELECT " (join-columns-name columns)
                  "FROM \"" table-name "\" "
                  "WHERE \"" (column-name id-column) "\" = ?1;")
                ,(column-symbol id-column))))

          ;; selects all rows
          (define (,(symbol-append table-symbol '-select-all) sql-connection)
            (map
              (lambda (row)
                (apply ,(symbol-append 'make- row-symbol) (sql-upgrade-row row columns)))
              (sql-read sql-connection
                ,(string-append
                  "SELECT " (join-columns-name columns)
                  "FROM \"" table-name "\" "))))

          ;; updates a row
          (define (,(symbol-append table-symbol '-update) sql-connection ,row-symbol)
            (sql-execute sql-connection
              (string-append
                "UPDATE \"" ,table-name "\" "
                "SET " (join-columns-variable-assignation value-columns)
                "WHERE \"" ,(column-name id-column) "\" = ?1;")
              ,@(map
                (lambda (column)
                  `(sql-downgrade-value (,(symbol-append row-symbol '- (column-symbol column)) ,row-symbol)))
                columns)))

          ;; deletes a row
          (define (,(symbol-append table-symbol '-delete) sql-connection ,row-symbol)
            (sql-execute sql-connection
              (string-append
                "DELETE "
                "FROM \"" ,table-name "\" "
                "WHERE \"" ,(column-name id-column) "\" = ?1;")
              (,(symbol-append row-symbol '- (column-symbol id-column)) ,row-symbol))))))))
