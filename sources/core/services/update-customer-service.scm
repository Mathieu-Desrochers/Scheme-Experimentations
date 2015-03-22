
(use srfi-1)
(use srfi-69)

(declare (unit update-customer-service))

(declare (uses compare))
(declare (uses customers-table))
(declare (uses hash))
(declare (uses list))
(declare (uses validation))
(declare (uses validation-service-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request update-customer-request
  (customer-id integer #t 1 999999)
  (first-name string #t 1 50)
  (last-name string #t 1 50)
  (birthdate date #t)
  (shipping-addresses list #t 1 100 (shipping-address update-customer-shipping-address-subrequest #t)))

(define-request update-customer-shipping-address-subrequest
  (shipping-address-id integer #f 1 999999)
  (street string #t 1 100)
  (city string #t 1 50)
  (state string #t 1 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response update-customer-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (update-customer-service sql-connection update-customer-request)

  ;; validate the request
  (validate-request update-customer-request validate-update-customer-request)

  ;; select and validate the customer-row
  (select-one-and-validate
    (customer-row
      customers-table-select-by-customer-id
      (update-customer-request-customer-id update-customer-request))
    (unknown-customer-id)

    ;; update the customer-row
    (customers-table-update
      sql-connection
      (make-customer-row
        (update-customer-request-customer-id update-customer-request)
        (update-customer-request-first-name update-customer-request)
        (update-customer-request-last-name update-customer-request)
        (update-customer-request-birthdate update-customer-request))))

  ;; select the shipping-address-rows
  (select-many
    (shipping-address-rows
      shipping-addresses-table-select-by-customer-id
      (update-customer-request-customer-id update-customer-request))

    ;; update the modified shipping-address-rows
    (update-modified-rows
      (update-customer-request-shipping-addresses
        update-customer-request
        update-customer-shipping-address-subrequest-shipping-address-id
        update-customer-shipping-address-subrequest-street
        update-customer-shipping-address-subrequest-city
        update-customer-shipping-address-subrequest-state)
      (shipping-address-rows
        shipping-address-row-shipping-address-id
        shipping-address-row-street
        shipping-address-row-city
        shipping-address-row-state)
      (shipping-addresses-table)

      ;; makes a new shipping-address-row
      (lambda (shipping-address-subrequest)
        (make-shipping-address-row
          0
          (update-customer-request-customer-id update-customer-request)
          (update-customer-shipping-address-subrequest-street shipping-address-subrequest)
          (update-customer-shipping-address-subrequest-city shipping-address-subrequest)
          (update-customer-shipping-address-subrequest-state shipping-address-subrequest)))

      ;; makes an updated shipping-address-row
      (lambda (shipping-address-subrequest shipping-address-row)
        (make-shipping-address-row
          (shipping-address-row-shipping-address-id shipping-address-row)
          (shipping-address-row-customer-id shipping-address-row)
          (update-customer-shipping-address-subrequest-street shipping-address-subrequest)
          (update-customer-shipping-address-subrequest-city shipping-address-subrequest)
          (update-customer-shipping-address-subrequest-state shipping-address-subrequest)))))

  ;; make the update-customer-response
  (make-update-customer-response))
