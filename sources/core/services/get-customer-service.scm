
(declare (unit get-customer-service))

(declare (uses customers-table))
(declare (uses date-time))
(declare (uses shipping-addresses-table))
(declare (uses validation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request get-customer-request
  (customer-id integer #t 1 1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response get-customer-response
  (customer-id)
  (first-name)
  (last-name)
  (is-vip)
  (shipping-address get-customer-shipping-address-subresponse))

(define-response get-customer-shipping-address-subresponse
  (shipping-address-id)
  (street)
  (city)
  (state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (get-customer-service sql-connection get-customer-request)

  ;; validate the request
  (validate-request get-customer-request validate-get-customer-request)

  ;; select the customer-row
  (let* ((customer-id (get-customer-request-customer-id get-customer-request))
         (customer-rows (customers-table-select-by-customer-id sql-connection customer-id)))
    (when (null? customer-rows)
      (abort-validation-error 'customer-id-unknown))

    ;; select the effective shipping-address-row
    (let* ((customer-row (car customer-rows))
           (shipping-address-rows (shipping-addresses-table-select-effective-by-customer-id sql-connection customer-id (date-now)))
           (effective-shipping-address-row (car shipping-address-rows)))

      ;; make the response
      (make-get-customer-response
        (customer-row-customer-id customer-row)
        (customer-row-first-name customer-row)
        (customer-row-last-name customer-row)
        (customer-row-is-vip customer-row)
        (make-get-customer-shipping-address-subresponse
          (shipping-address-row-shipping-address-id effective-shipping-address-row)
          (shipping-address-row-street effective-shipping-address-row)
          (shipping-address-row-city effective-shipping-address-row)
          (shipping-address-row-state effective-shipping-address-row))))))
