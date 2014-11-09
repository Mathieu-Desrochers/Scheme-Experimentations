
(use srfi-1)
(use srfi-69)

(declare (unit update-shipping-address-service))

(declare (uses compare))
(declare (uses customers-table))
(declare (uses hash))
(declare (uses list))
(declare (uses shipping-addresses-table))
(declare (uses validation))
(declare (uses validation-service-request))

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

  ;; select and validate the customer-row
  (select-one-and-validate
    (customer-row
      customers-table-select-by-customer-id
      (update-shipping-address-request-customer-id update-shipping-address-request))
    (unknown-customer-id)

    ;; select and validate the shipping-address-row
    (select-one-and-validate
      (shipping-address-row
        shipping-addresses-table-select-by-shipping-address-id
        (update-shipping-address-request-shipping-address-id update-shipping-address-request))
      (unknown-shipping-address-id)

      ;; validate the shipping-address-row
      ;; belongs to the customer
      (unless (equal? (customer-row-customer-id customer-row) (shipping-address-row-customer-id shipping-address-row))
        (abort-validation-error 'shipping-address-id-unknown))

        ;; update the shipping-address-row
        (shipping-addresses-table-update
          sql-connection
          (make-shipping-address-row
            (update-shipping-address-request-shipping-address-id update-shipping-address-request)
            (update-shipping-address-request-customer-id update-shipping-address-request)
            (update-shipping-address-request-effective-date update-shipping-address-request)
            (update-shipping-address-request-street update-shipping-address-request)
            (update-shipping-address-request-city update-shipping-address-request)
            (update-shipping-address-request-state update-shipping-address-request)))

        ;; make the update-shipping-address-response
        (make-update-shipping-address-response))))
