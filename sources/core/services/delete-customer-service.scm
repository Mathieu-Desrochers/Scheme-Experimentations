
(declare (unit delete-customer-service))

(declare (uses customers-table))
(declare (uses shipping-addresses-table))
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

  ;; validate the customer-id
  (let* ((customer-id (delete-customer-request-customer-id delete-customer-request))
         (customer-rows (customers-table-select-by-customer-id sql-connection customer-id)))
    (when (null? customer-rows)
      (abort-validation-error 'customer-id-unknown))

    ;; delete the shipping-address-rows
    (shipping-addresses-table-delete-by-customer-id sql-connection customer-id)

    ;; delete the customer-row
    (customers-table-delete sql-connection (car customer-rows))

    ;; make the response
    (make-delete-customer-response)))
