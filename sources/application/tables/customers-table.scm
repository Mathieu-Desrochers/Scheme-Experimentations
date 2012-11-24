
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table definition

(define-table customers-table "customers" customer-row ("customer-id" "first-name" "last-name"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test bindings

(declare (uses sql))

(define (display-customer-rows sql-connection)
  (define (display-customer-rows-iter customer-rows)
    (when (not (null? customer-rows))
      (let ((customer-row (car customer-rows)))
        (display (customer-row-customer-id customer-row))
        (display "|")
        (display (customer-row-first-name customer-row))
        (display "|")
        (display (customer-row-last-name customer-row))
        (newline)
        (display-customer-rows-iter (cdr customer-rows)))))
  (let ((customer-rows (customers-table-select-all sql-connection)))
    (display-customer-rows-iter customer-rows)))

(with-sql-connection "database.db"
  (lambda (sql-connection)
    (let ((customer-row (make-customer-row 0 "Mathieu" "Desrochers")))
      (customers-table-insert sql-connection customer-row)
      (display-customer-rows sql-connection)
      (customer-row-first-name-set! customer-row "Alice")
      (customer-row-last-name-set! customer-row "Deschamps")
      (customers-table-update sql-connection customer-row)
      (display-customer-rows sql-connection)
      (customers-table-delete sql-connection customer-row)
      (display-customer-rows sql-connection)
      )))
