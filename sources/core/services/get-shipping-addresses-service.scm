
(use srfi-1)
(use srfi-69)

(declare (unit get-shipping-addresses-service))

(declare (uses compare))
(declare (uses customers-table))
(declare (uses hash))
(declare (uses list))
(declare (uses shipping-addresses-table))
(declare (uses validation))
(declare (uses validation-service-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request get-shipping-addresses-request
  (customer-id integer #t 1 1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response get-shipping-addresses-response
  (shipping-addresses list (shipping-address get-shipping-addresses-shipping-address-subresponse)))

(define-response get-shipping-addresses-shipping-address-subresponse
  (shipping-address-id integer)
  (effective-date date)
  (street string)
  (city string)
  (state string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (get-shipping-addresses-service sql-connection get-shipping-addresses-request)

  ;; validate the request
  (validate-request get-shipping-addresses-request validate-get-shipping-addresses-request)

  ;; select and validate the customer-row
  (select-one-and-validate
    (customer-row
      customers-table-select-by-customer-id
      (get-shipping-addresses-request-customer-id get-shipping-addresses-request))
    (unknown-customer-id)

    ;; select the shipping-address-rows
    (select-many
      (shipping-address-rows
        shipping-addresses-table-select-by-customer-id
        (get-shipping-addresses-request-customer-id get-shipping-addresses-request))

      ;; make the get-shipping-addresses-response
      (make-get-shipping-addresses-response
        (map
          (lambda (shipping-address-row)

            ;; make the get-shipping-addresses
            ;; shipping-address-subresponses
            (make-get-shipping-addresses-shipping-address-subresponse
              (shipping-address-row-shipping-address-id shipping-address-row)
              (shipping-address-row-effective-date shipping-address-row)
              (shipping-address-row-street shipping-address-row)
              (shipping-address-row-city shipping-address-row)
              (shipping-address-row-state shipping-address-row)))

          shipping-address-rows)))))
