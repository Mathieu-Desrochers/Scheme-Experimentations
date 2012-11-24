
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service definition

;; declarations
(declare (uses sql))
(declare (uses customers-table))

;; encapsulates a service request
(define-record new-customer-request first-name last-name)

;; encapsulates a service response
(define-record new-customer-response customer-id)

;; executes the service
(define (new-customer-service new-customer-request)
  (with-sql-connection "database.db"
    (lambda (sql-connection)
      (let ((customer-row
              (make-customer-row
                0
                (new-customer-request-first-name new-customer-request)
                (new-customer-request-last-name new-customer-request))))
        (customers-table-insert sql-connection customer-row)
        (make-new-customer-response
          (customer-row-customer-id customer-row))))))

(new-customer-service (make-new-customer-request "Mathieu" "Desrochers"))
