
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

      ;; upgrades a row based on its columns type
      (define (sql-upgrade-row columns)
        `(list
          ,@(map
            (lambda (column-index)
              `(sql-upgrade-value
                (list-ref row ,column-index)
                ',(column-type (list-ref columns column-index))))
            (iota (length columns)))))

      ;; makes a row based on its columns type
      (define (sql-make-row row-symbol columns)
        `(lambda (row)
          (apply
            ,(symbol-append 'make- row-symbol)
            ,(sql-upgrade-row columns))))

      ;; parses the expression
      (let* ((table-symbol (car (list-ref exp 1)))
             (table-name (cadr (list-ref exp 1)))
             (row-symbol (car (list-ref exp 2)))
             (columns (make-columns (cdr (list-ref exp 2))))
             (id-column (car columns))
             (value-columns (cdr columns))
             (custom-reads (cdr (list-ref exp 3))))
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
              ,(sql-make-row row-symbol columns)
              (sql-read sql-connection
                ,(string-append
                  "SELECT * "
                  "FROM \"" table-name "\" "
                  "WHERE \"" (column-name id-column) "\" = ?1;")
                ,(column-symbol id-column))))

          ;; selects all rows
          (define (,(symbol-append table-symbol '-select-all) sql-connection)
            (map
              ,(sql-make-row row-symbol columns)
              (sql-read sql-connection
                ,(string-append
                  "SELECT * "
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
              (,(symbol-append row-symbol '- (column-symbol id-column)) ,row-symbol)))

          ;; custom reads
          ,@(map
            (lambda (custom-read)
              (let ((custom-read-symbol (car custom-read))
                    (custom-read-sql (cadr custom-read))
                    (custom-read-parameters (cddr custom-read)))
                `(define (,custom-read-symbol sql-connection ,@custom-read-parameters)
                  (map
                    ,(sql-make-row row-symbol columns)
                    (sql-read sql-connection
                      ,custom-read-sql
                      ,@(map
                        (lambda (custom-read-parameter)
                          `(sql-downgrade-value ,custom-read-parameter))
                        custom-read-parameters))))))
            custom-reads))))))
