
(declare (uses sql))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public bindings

;; encapsulates a customer row
(define-record customer-row customer-id first-name last-name)

;; inserts a customer row
(define (customers-table-insert sql-connection customer-row)
  (sql-execute sql-connection
    "INSERT INTO \"customers\" (\"customer-id\", \"first-name\", \"last-name\") VALUES (?1, ?2, ?3);"
    (customer-row-customer-id customer-row)
    (customer-row-first-name customer-row)
    (customer-row-last-name customer-row)))

;; selects customer rows by customer id
(define (customers-table-select-by-customer-id sql-connection customer-id)
  (map
    (lambda (row)
      (apply make-customer-row row))
    (sql-read sql-connection
      "SELECT * FROM \"customers\" WHERE \"customer-id\" = ?1;"
      customer-id)))

;; updates a customer row
(define (customers-table-update sql-connection customer-row)
  (sql-execute sql-connection
    "UPDATE \"customers\" SET \"first-name\" = ?2, \"last-name\" = ?3 WHERE \"customer-id\" = ?1;"
    (customer-row-customer-id customer-row)
    (customer-row-first-name customer-row)
    (customer-row-last-name customer-row)))

;; deletes a customer row
(define (customers-table-delete sql-connection customer-row)
  (sql-execute sql-connection
    "DELETE FROM \"customers\" WHERE \"customer-id\" = ?1;"
    (customer-row-customer-id customer-row)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test bindings

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
  (let ((customer-rows (customers-table-select-by-customer-id sql-connection 1000)))
    (display-customer-rows-iter customer-rows)))

(with-sql-connection "database.db"
  (lambda (sql-connection)
    (let ((customer-row (make-customer-row 1000 "Mathieu" "Desrochers")))
      (customers-table-insert sql-connection customer-row)
      (display-customer-rows sql-connection)
      (customer-row-first-name-set! customer-row "Alice")
      (customer-row-last-name-set! customer-row "Deschamps")
      (customers-table-update sql-connection customer-row)
      (display-customer-rows sql-connection)
      (customers-table-delete sql-connection customer-row)
      (display-customer-rows sql-connection))))
