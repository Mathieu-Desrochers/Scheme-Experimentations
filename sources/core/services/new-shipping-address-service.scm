
(use srfi-1)
(use srfi-69)

(declare (unit new-shipping-address-service))

(declare (uses compare))
(declare (uses customers-table))
(declare (uses date-time))
(declare (uses hash))
(declare (uses list))
(declare (uses shipping-addresses-table))
(declare (uses validation))
(declare (uses validation-service-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request new-shipping-address-request
  (customer-id integer #t 1 1000000)
  (effective-date date #t)
  (street string #t 1 100)
  (city string #t 1 50)
  (state string #t 1 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response new-shipping-address-response
  (shipping-address-id integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (new-shipping-address-service sql-connection new-shipping-address-request)

  ;; validate the request
  (validate-request new-shipping-address-request validate-new-shipping-address-request)

  ;; select and validate the customer-row
  (select-one-and-validate
    (customer-row
      customers-table-select-by-customer-id
      (new-shipping-address-request-customer-id new-shipping-address-request))
    (unknown-customer-id)

    ;; insert a shipping-address-row
    (let ((shipping-address-id
            (shipping-addresses-table-insert
              sql-connection
              (make-shipping-address-row
                0
                (new-shipping-address-request-customer-id new-shipping-address-request)
                (new-shipping-address-request-effective-date new-shipping-address-request)
                (new-shipping-address-request-street new-shipping-address-request)
                (new-shipping-address-request-city new-shipping-address-request)
                (new-shipping-address-request-state new-shipping-address-request)))))

      ;; make the new-shipping-address-response
      (make-new-shipping-address-response
        shipping-address-id))))
