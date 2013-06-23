
(declare (unit get-shipping-addresses-service))

(declare (uses customers-table))
(declare (uses shipping-addresses-table))
(declare (uses validation-service-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request get-shipping-addresses-request
  (customer-id integer #t 1 1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response get-shipping-addresses-response
  (shipping-addresses list get-shipping-addresses-subresponse))

(define-response get-shipping-addresses-subresponse
  (shipping-address-id)
  (effective-date)
  (street)
  (city)
  (state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (get-shipping-addresses-service sql-connection get-shipping-addresses-request)

  ;; validate the request
  (validate-request get-shipping-addresses-request validate-get-shipping-addresses-request)

  ;; select the customer-row
  (let* ((customer-id (get-shipping-addresses-request-customer-id get-shipping-addresses-request))
         (customer-rows (customers-table-select-by-customer-id sql-connection customer-id)))
    (when (null? customer-rows)
      (abort-validation-error 'customer-id-unknown))

    ;; select the shipping-address-rows
    (let ((shipping-address-rows (shipping-addresses-table-select-by-customer-id sql-connection customer-id)))

      ;; make the response
      (make-get-shipping-addresses-response
        (map
          (lambda (shipping-address-row)
            (make-get-shipping-addresses-subresponse
              (shipping-address-row-shipping-address-id shipping-address-row)
              (shipping-address-row-effective-date shipping-address-row)
              (shipping-address-row-street shipping-address-row)
              (shipping-address-row-city shipping-address-row)
              (shipping-address-row-state shipping-address-row)))
          shipping-address-rows)))))
