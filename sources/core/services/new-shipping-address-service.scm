
(declare (unit new-shipping-address-service))

(declare (uses customers-table))
(declare (uses datetime))
(declare (uses shipping-addresses-table))
(declare (uses validation))

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
  (shipping-address-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (new-shipping-address-service sql-connection new-shipping-address-request)

  ;; validate the request
  (validate-request new-shipping-address-request validate-new-shipping-address-request)

  ;; validate the customer-id
  (let* ((customer-id (new-shipping-address-request-customer-id new-shipping-address-request))
         (customer-row (customers-table-select-by-customer-id sql-connection customer-id)))
    (when (null? customer-row)
      (abort-validation-error 'customer-id-unknown))

    ;; insert a shipping-address-row
    (let ((shipping-address-id
            (shipping-addresses-table-insert
              sql-connection
              (make-shipping-address-row 0
                customer-id
                (new-shipping-address-request-effective-date new-shipping-address-request)
                (new-shipping-address-request-street new-shipping-address-request)
                (new-shipping-address-request-city new-shipping-address-request)
                (new-shipping-address-request-state new-shipping-address-request)))))

      ;; make the response
      (make-new-shipping-address-response shipping-address-id))))
