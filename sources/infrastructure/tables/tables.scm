
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table definition

(define-syntax define-table
  (er-macro-transformer
    (lambda (exp rename compare)
      (let ((table-name (cadr exp))
            (row-name (caddr exp))
            (columns-name (cadddr exp)))
        `(begin

          ;; encapsulates a row
          (define-record ,(symbol-append row-name '-row) ,@columns-name)

          ;; inserts a row
          (define (,(symbol-append table-name '-table-insert) sql-connection ,(symbol-append row-name '-row))
            (sql-execute sql-connection
              (string-append
                "INSERT INTO \"" ,(symbol->string table-name) "\" ("
                ,(string-join
                  (map
                    (lambda (column-name)
                      (string-append "\"" (symbol->string column-name) "\""))
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
                  `(,(symbol-append row-name '-row- column-name) ,(symbol-append row-name '-row)))
                columns-name)))

          ;; selects a row by id
          (define (,(symbol-append table-name '-table-select-by- (car columns-name)) sql-connection ,(car columns-name)) 
            (map
              (lambda (row)
                (apply ,(symbol-append 'make- row-name '-row) row))
              (sql-read sql-connection
                ,(string-append
                  "SELECT * FROM \"" (symbol->string table-name) "\" "
                  "WHERE \"" (symbol->string (car columns-name)) "\" = ?1;")
                ,(car columns-name))))

          ;; updates a row
          (define (,(symbol-append table-name '-table-update) sql-connection ,(symbol-append row-name '-row))
            (sql-execute sql-connection
              (string-append
                "UPDATE \"" ,(symbol->string table-name) "\" SET "
                ,(string-join
                  (map
                    (lambda (column-index)
                      (let ((column-number (+ column-index 1)))
                        (string-append
                          "\"" (symbol->string (list-ref columns-name column-index)) "\" = "
                          "?" (number->string column-number))))
                    (iota (- (length columns-name) 1) 1))
                  ", ")
                " WHERE \"" ,(symbol->string (car columns-name)) "\" = ?1;")
              ,@(map
                (lambda (column-name)
                  `(,(symbol-append row-name '-row- column-name) ,(symbol-append row-name '-row)))
                columns-name)))

          ;; deletes a row
          (define (,(symbol-append table-name '-table-delete) sql-connection ,(symbol-append row-name '-row))
            (sql-execute sql-connection
              (string-append
                "DELETE FROM \"" ,(symbol->string table-name) "\" "
                "WHERE \"" ,(symbol->string (car columns-name)) "\" = ?1;")
              (,(symbol-append row-name '-row- (car columns-name)) ,(symbol-append row-name '-row)))))))))
