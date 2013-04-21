
(declare (unit update-shipping-address-service))

(declare (uses customers-table))
(declare (uses shipping-addresses-table))
(declare (uses validation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request update-shipping-address-request
  (customer-id integer #t 1 1000000)
  (shipping-address-id integer #t 1 1000000)
  (effective-date date #t)
  (street string #t 1 100)
  (city string #t 1 50)
  (state string #t 1 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response update-shipping-address-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (update-shipping-address-service sql-connection update-shipping-address-request)

  ;; validate the request
  (validate-request update-shipping-address-request validate-update-shipping-address-request)

  ;; validate the customer-id
  (let* ((customer-id (update-shipping-address-request-customer-id update-shipping-address-request))
         (customer-rows (customers-table-select-by-customer-id sql-connection customer-id)))
    (when (null? customer-rows)
      (abort-validation-error 'customer-id-unknown))

    ;; validate the shipping-address-id
    (let* ((shipping-address-id (update-shipping-address-request-shipping-address-id update-shipping-address-request))
           (shipping-address-rows (shipping-addresses-table-select-by-shipping-address-id sql-connection shipping-address-id)))
      (when (null? shipping-address-rows)
        (abort-validation-error 'shipping-address-id-unknown))

      ;; validate the customer-id / shipping-address-id relation
      (let* ((shipping-address-row (car shipping-address-rows))
             (shipping-address-row-customer-id (shipping-address-row-customer-id shipping-address-row)))
        (when (not (equal? customer-id shipping-address-row-customer-id))
          (abort-validation-error 'shipping-address-id-unknown))

        ;; update the shipping-address-row
        (shipping-addresses-table-update
          sql-connection
          (make-shipping-address-row
            shipping-address-id
            customer-id
            (update-shipping-address-request-effective-date update-shipping-address-request)
            (update-shipping-address-request-street update-shipping-address-request)
            (update-shipping-address-request-city update-shipping-address-request)
            (update-shipping-address-request-state update-shipping-address-request)))

        ;; make the response
        (make-update-shipping-address-response)))))
