
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; select one

(define-syntax select-one
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((row-expression (cadr exp))
             (row-symbol (list-ref row-expression 0))
             (table-select-by-symbol (list-ref row-expression 1))
             (table-select-by-parameter-symbols (drop row-expression 2))
             (body (cddr exp)))

        ;; select the rows
        `(let ((,(rename 'rows)
                 (,table-select-by-symbol
                    sql-connection
                    ,@table-select-by-parameter-symbols)))

          ;; bring the selected row into scope
          ;; and execute the body
          (let ((,row-symbol (if (null? ,(rename 'rows)) #f (car ,(rename 'rows)))))

           ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; select one and validate

(define-syntax select-one-and-validate
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((row-expression (cadr exp))
             (row-symbol (list-ref row-expression 0))
             (table-select-by-symbol (list-ref row-expression 1))
             (table-select-by-parameter-symbols (drop row-expression 2))
             (validation-error-expression (caddr exp))
             (validation-error-symbol (list-ref validation-error-expression 0))
             (body (cdddr exp)))

        ;; select the rows
        `(let ((,(rename 'rows)
                 (,table-select-by-symbol
                    sql-connection
                    ,@table-select-by-parameter-symbols)))

          ;; validate a row was selected
          (when (null? ,(rename 'rows))
            (abort-validation-error (quote ,validation-error-symbol)))

          ;; bring the selected row into scope
          ;; and execute the body
          (let ((,row-symbol (car ,(rename 'rows))))

           ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; select many

(define-syntax select-many
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((rows-expression (cadr exp))
             (rows-symbol (list-ref rows-expression 0))
             (table-select-by-symbol (list-ref rows-expression 1))
             (table-select-by-parameter-symbols (drop rows-expression 2))
             (body (cddr exp)))

        ;; select the rows
        ;; and bring them into scope
        `(let ((,rows-symbol
                (,table-select-by-symbol
                  sql-connection
                  ,@table-select-by-parameter-symbols)))

          ;; execute the body
          ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; select many and hash

(define-syntax select-many-and-hash
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((rows-expression (cadr exp))
             (rows-symbol (list-ref rows-expression 0))
             (table-select-by-symbol (list-ref rows-expression 1))
             (table-select-by-parameter-symbols (drop rows-expression 2))
             (rows-hash-table-expression (caddr exp))
             (rows-hash-table-symbol (list-ref rows-hash-table-expression 0))
             (hash-with-keys-symbol (list-ref rows-hash-table-expression 1))
             (hash-row-value-symbol (list-ref rows-hash-table-expression 2))
             (get-row-expression (cadddr exp))
             (get-row-symbol (list-ref get-row-expression 0))
             (get-row-from-value-symbol (list-ref get-row-expression 1))
             (get-row-default (list-ref get-row-expression 2))
             (body (cddddr exp)))

        ;; select the rows
        `(let ((,rows-symbol
                (,table-select-by-symbol
                  sql-connection
                  ,@table-select-by-parameter-symbols)))

          ;; hash the selected rows
          (let ((,rows-hash-table-symbol
                  (,hash-with-keys-symbol
                    ,rows-symbol
                    ,hash-row-value-symbol
                    identity)))

            ;; retrieves an entry from the hash table
            (let ((,get-row-symbol
                    (lambda (,(rename 'get-row-from))
                      (let ((,(rename 'get-row-from-value) (,get-row-from-value-symbol ,(rename 'get-row-from))))
                        (if ,(rename 'get-row-from-value)
                          (hash-table-ref/default
                            ,rows-hash-table-symbol
                            ,(rename 'get-row-from-value)
                            ,get-row-default)
                          ,get-row-default)))))

              ;; execute the body
              ,@body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make subresponses

(define-syntax make-subresponses
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((rows-expression (cadr exp))
             (rows-symbol (list-ref rows-expression 0))
             (list-sort-by-symbol (list-ref rows-expression 1))
             (sort-by-value-symbol (list-ref rows-expression 2))
             (make-subresponse-lambda (caddr exp)))

        ;; map the make-subresponse-symbol to every row
        `(map
          ,make-subresponse-lambda

          ;; sort the rows to provide a predictable and repeatable
          ;; ordering of the subresponses
          (,list-sort-by-symbol
            ,rows-symbol
            ,sort-by-value-symbol))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; validate duplicates

(define-syntax validate-duplicates
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (cadr exp))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-value-symbol (list-ref subrequests-expression 2))
             (validation-errors-expression (caddr exp))
             (validation-errors-first-symbol (list-ref validation-errors-expression 0))
             (validation-errors-second-symbol (list-ref validation-errors-expression 1)))

        ;; get the duplicates index
        `(let ((,(rename 'duplicates-index)
                (list-duplicates-index
                  (,request-value-symbol ,request-symbol)
                  ,subrequest-value-symbol)))

          ;; make sure there are no duplicates index
          (unless (null? ,(rename 'duplicates-index))
            (abort-validation-errors
              (make-numbered-validation-errors
                (quote ,validation-errors-first-symbol)
                ,(rename 'duplicates-index)
                (symbol-append '- (quote ,validation-errors-second-symbol))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; validate references

(define-syntax validate-references
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (cadr exp))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-value-symbol (list-ref subrequests-expression 2))
             (rows-expression (caddr exp))
             (rows-symbol (list-ref rows-expression 0))
             (row-value-symbol (list-ref rows-expression 1))
             (validation-errors-expression (cadddr exp))
             (validation-errors-first-symbol (list-ref validation-errors-expression 0))
             (validation-errors-second-symbol (list-ref validation-errors-expression 1)))

        ;; get the non-matches index
        `(let ((,(rename 'non-matches-index)
                (list-non-matches-index
                  (,request-value-symbol ,request-symbol)
                  ,subrequest-value-symbol
                  ,rows-symbol
                  ,row-value-symbol)))

          ;; make sure there are no non-matches index
          (unless (null? ,(rename 'non-matches-index))
            (abort-validation-errors
              (make-numbered-validation-errors
                (quote ,validation-errors-first-symbol)
                ,(rename 'non-matches-index)
                (symbol-append '- (quote ,validation-errors-second-symbol))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; validates the inserted rows

(define-syntax validate-inserted-rows
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (cadr exp))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-id-symbol (list-ref subrequests-expression 2))
             (validation-errors-expression (caddr exp))
             (validation-errors-first-symbol (list-ref validation-errors-expression 0))
             (validation-errors-second-symbol (list-ref validation-errors-expression 1))
             (validate-insert-lambda (cadddr exp)))

        ;; get the invalid subrequests index
        `(let ((,(rename 'invalid-subrequests-index)
                  (list-filtered-index
                    (,request-value-symbol ,request-symbol)
                     identity
                     (lambda (,(rename 'subrequest))

                        ;; get the subrequest id
                        (let ((,(rename 'subrequest-id) (,subrequest-id-symbol ,(rename 'subrequest))))
                          (if (not ,(rename 'subrequest-id))

                            ;; validate the insert
                            (not (,validate-insert-lambda ,(rename 'subrequest)))

                            ;; the subrequest has an id
                            ;; and is therefore not an insert
                            #f))))))

           ;; make sure there are no invalid subrequests index
           (unless (null? ,(rename 'invalid-subrequests-index))
             (abort-validation-errors
               (make-numbered-validation-errors
                 (quote ,validation-errors-first-symbol)
                 ,(rename 'invalid-subrequests-index)
                 (symbol-append '- (quote ,validation-errors-second-symbol))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; validates the updated rows

(define-syntax validate-updated-rows
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (cadr exp))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-id-symbol (list-ref subrequests-expression 2))
             (rows-expression (caddr exp))
             (rows-symbol (list-ref rows-expression 0))
             (row-id-symbol (list-ref rows-expression 1))
             (validation-errors-expression (cadddr exp))
             (validation-errors-first-symbol (list-ref validation-errors-expression 0))
             (validation-errors-second-symbol (list-ref validation-errors-expression 1))
             (validate-update-lambda (list-ref exp 4)))

        ;; hash the rows
        `(let ((,(rename 'rows-hash-table)
                 (hash-with-unique-numeric-keys
                   ,rows-symbol
                   ,row-id-symbol
                   identity)))

          ;; get the invalid subrequests index
          (let ((,(rename 'invalid-subrequests-index)
                  (list-filtered-index
                    (,request-value-symbol ,request-symbol)
                    identity
                    (lambda (,(rename 'subrequest))

                      ;; get the subrequest id
                      (let ((,(rename 'subrequest-id) (,subrequest-id-symbol ,(rename 'subrequest))))
                        (if ,(rename 'subrequest-id)

                          ;; get the matching row
                          (let ((,(rename 'matching-row)
                                  (hash-table-ref/default
                                    ,(rename 'rows-hash-table)
                                    ,(rename 'subrequest-id)
                                    #f)))

                            ;; validate the update
                            (if ,(rename 'matching-row)
                              (not (,validate-update-lambda ,(rename 'subrequest) ,(rename 'matching-row)))

                              ;; the subrequest does not have a matching row
                              ;; and is therefore not an update
                              #f))

                          ;; the subrequest does not have an id
                          ;; and is therefore not an update
                          #f))))))

            ;; make sure there are no invalid subrequests index
            (unless (null? ,(rename 'invalid-subrequests-index))
              (abort-validation-errors
                (make-numbered-validation-errors
                  (quote ,validation-errors-first-symbol)
                  ,(rename 'invalid-subrequests-index)
                  (symbol-append '- (quote ,validation-errors-second-symbol)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; validates the deleted rows

(define-syntax validate-deleted-rows
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (cadr exp))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-id-symbol (list-ref subrequests-expression 2))
             (rows-expression (caddr exp))
             (rows-symbol (list-ref rows-expression 0))
             (row-id-symbol (list-ref rows-expression 1))
             (validation-errors-expression (cadddr exp))
             (validation-error-symbol (list-ref validation-errors-expression 0))
             (validate-delete-lambda (list-ref exp 4)))

        ;; hash the subrequests having an id
        `(let ((,(rename 'subrequests-hash-table)
                 (hash-with-unique-numeric-keys
                   (filter ,subrequest-id-symbol (,request-value-symbol ,request-symbol))
                   ,subrequest-id-symbol
                   identity)))

          ;; get the invalid rows index
          (let ((,(rename 'invalid-rows-index)
                  (list-filtered-index
                    ,rows-symbol
                    identity
                    (lambda (,(rename 'row))

                      ;; get the matching subrequest
                      (let ((,(rename 'matching-subrequest)
                              (hash-table-ref/default
                                ,(rename 'subrequests-hash-table)
                                (,row-id-symbol ,(rename 'row))
                                #f)))

                        ;; validate the delete
                        (if (not ,(rename 'matching-subrequest))
                          (not (,validate-delete-lambda ,(rename 'row)))

                          ;; the row has a matching subrequest
                          ;; and is therefore not an delete
                          #f))))))

            ;; make sure there are no invalid subrequests index
            (unless (null? ,(rename 'invalid-rows-index))
              (abort-validation-error (quote ,validation-error-symbol)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update modified rows

(define-syntax update-modified-rows
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (cadr exp))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-id-symbol (list-ref subrequests-expression 2))
             (subrequest-value-symbols (drop subrequests-expression 3))
             (rows-expression (caddr exp))
             (rows-symbol (list-ref rows-expression 0))
             (row-id-symbol (list-ref rows-expression 1))
             (row-value-symbols (drop rows-expression 2))
             (table-expression (cadddr exp))
             (table-symbol (list-ref table-expression 0))
             (make-inserted-row-expression (list-ref exp 4))
             (make-updated-row-expression (list-ref exp 5)))

        ;; compare the elements
        `(let (

            (compare-results
              (compare-elements

                ;; the original rows
                ,rows-symbol
                ,row-id-symbol

                ;; the current subrequests
                (,request-value-symbol ,request-symbol)
                ,subrequest-id-symbol

                ;; returns whether an element has changed
                (lambda (row subrequest)
                  (not
                    (equal?
                      (list
                        ,@(map
                            (lambda (row-value-symbol)
                              `(,row-value-symbol row))
                            row-value-symbols))
                      (list
                        ,@(map
                            (lambda (subrequest-value-symbol)
                              `(,subrequest-value-symbol subrequest))
                            subrequest-value-symbols)))))

                ;; makes an added row
                ,make-inserted-row-expression

                ;; makes a changed row
                (lambda (,(rename 'row) ,(rename 'subrequest))
                  (,make-updated-row-expression ,(rename 'subrequest) ,(rename 'row)))

                ;; makes an unchanged row
                (lambda (row subrequest) row)

                ;; makes a deleted row
                identity)))

          ;; insert the added rows
          (for-each
            (lambda (row)
              (,(symbol-append table-symbol '-insert)
                sql-connection
                row))
            (compare-results-added-elements
              compare-results))

          ;; update the changed rows
          (for-each
            (lambda (row)
              (,(symbol-append table-symbol '-update)
                sql-connection
                row))
            (compare-results-changed-elements
              compare-results))

          ;; delete the deleted rows
          (for-each
            (lambda (row)
              (,(symbol-append table-symbol '-delete)
                sql-connection
                row))
            (compare-results-deleted-elements
              compare-results)))))))
