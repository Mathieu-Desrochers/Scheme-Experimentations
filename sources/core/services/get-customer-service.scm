
(use srfi-1)
(use srfi-69)

(declare (unit get-customer-service))

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

(define-request get-customer-request
  (customer-id integer #t 1 1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response get-customer-response
  (customer-id integer)
  (first-name string)
  (last-name string)
  (is-vip boolean)
  (shipping-address get-customer-shipping-address-subresponse))

(define-response get-customer-shipping-address-subresponse
  (shipping-address-id integer)
  (street string)
  (city string)
  (state string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (get-customer-service sql-connection get-customer-request)

  ;; validate the request
  (validate-request get-customer-request validate-get-customer-request)

  ;; select and validate the customer-row
  (select-one-and-validate
    (customer-row
      customers-table-select-by-customer-id
      (get-customer-request-customer-id get-customer-request))
    (unknown-customer-id)

    ;; select the effective shipping-address-row
    (select-one
      (effective-shipping-address-row
        shipping-addresses-table-select-effective-by-customer-id
        (get-customer-request-customer-id get-customer-request)
        (date-now))

      ;; make the get-customer-response
      (make-get-customer-response
        (customer-row-customer-id customer-row)
        (customer-row-first-name customer-row)
        (customer-row-last-name customer-row)
        (customer-row-is-vip customer-row)

        ;; make the get-customer
        ;; shipping-address-subresponse
        (make-get-customer-shipping-address-subresponse
          (shipping-address-row-shipping-address-id effective-shipping-address-row)
          (shipping-address-row-street effective-shipping-address-row)
          (shipping-address-row-city effective-shipping-address-row)
          (shipping-address-row-state effective-shipping-address-row))))))
