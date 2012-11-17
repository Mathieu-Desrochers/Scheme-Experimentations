
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table definition

(define-syntax define-table
  (er-macro-transformer
    (lambda (exp rename compare)
      (let* ((table-symbol (list-ref exp 1))
             (table-name (list-ref exp 2))
             (row-symbol (list-ref exp 3))
             (columns-name (list-ref exp 4))
             (id-column-symbol (string->symbol (car columns-name)))
             (id-column-name (car columns-name)))
        `(begin

          ;; encapsulates a row
          (define-record ,row-symbol
            ,@(map
              (lambda (column-name)
                (string->symbol column-name))
              columns-name))

          ;; inserts a row
          (define (,(symbol-append table-symbol '-insert) sql-connection ,row-symbol)
            (sql-execute sql-connection
              (string-append
                "INSERT INTO \"" ,table-name "\" ("
                ,(string-join
                  (map
                    (lambda (column-name)
                      (string-append "\"" column-name "\""))
                    columns-name)
                  ", ")
                ") VALUES ("
                ,(string-join
                  (map
                    (lambda (column-number)
                      (string-append "?" (number->string column-number)))
                    (iota (length columns-name) 1))
                  ", ")
                ");")
              ,@(map
                (lambda (column-name)
                  `(,(symbol-append row-symbol '- (string->symbol column-name)) ,row-symbol))
                columns-name)))

          ;; selects a row by id
          (define (,(symbol-append table-symbol '-select-by- id-column-symbol) sql-connection ,id-column-symbol)
            (map
              (lambda (row)
                (apply ,(symbol-append 'make- row-symbol) row))
              (sql-read sql-connection
                ,(string-append
                  "SELECT * FROM \"" table-name "\" "
                  "WHERE \"" id-column-name "\" = ?1;")
                ,id-column-symbol)))

          ;; updates a row
          (define (,(symbol-append table-symbol '-update) sql-connection ,row-symbol)
            (sql-execute sql-connection
              (string-append
                "UPDATE \"" ,table-name "\" SET "
                ,(string-join
                  (map
                    (lambda (column-index)
                      (let ((column-number (+ column-index 1)))
                        (string-append
                          "\"" (list-ref columns-name column-index) "\" = "
                          "?" (number->string column-number))))
                    (iota (- (length columns-name) 1) 1))
                  ", ")
                " WHERE \"" ,id-column-name "\" = ?1;")
              ,@(map
                (lambda (column-name)
                  `(,(symbol-append row-symbol '- (string->symbol column-name)) ,row-symbol))
                columns-name)))

          ;; deletes a row
          (define (,(symbol-append table-symbol '-delete) sql-connection ,row-symbol)
            (sql-execute sql-connection
              (string-append
                "DELETE FROM \"" ,table-name "\" "
                "WHERE \"" ,id-column-name "\" = ?1;")
              (,(symbol-append row-symbol '- id-column-symbol) ,row-symbol))))))))
