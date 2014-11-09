
(use srfi-1)
(use srfi-69)

(declare (unit delete-customer-service))

(declare (uses compare))
(declare (uses customers-table))
(declare (uses hash))
(declare (uses list))
(declare (uses shipping-addresses-table))
(declare (uses validation))
(declare (uses validation-service-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request delete-customer-request
  (customer-id integer #t 1 1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response delete-customer-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (delete-customer-service sql-connection delete-customer-request)

  ;; validate the request
  (validate-request delete-customer-request validate-delete-customer-request)

  ;; select and validate the customer-row
  (select-one-and-validate
    (customer-row
      customers-table-select-by-customer-id
      (delete-customer-request-customer-id delete-customer-request))
    (unknown-customer-id)

    ;; delete the shipping-address-rows
    (shipping-addresses-table-delete-by-customer-id
      sql-connection
      (delete-customer-request-customer-id delete-customer-request))

    ;; delete the customer-row
    (customers-table-delete sql-connection customer-row)

    ;; make the delete-customer-response
    (make-delete-customer-response)))
