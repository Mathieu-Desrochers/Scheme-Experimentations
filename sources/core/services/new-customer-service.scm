
(use srfi-1)
(use srfi-69)

(declare (unit new-customer-service))

(declare (uses compare))
(declare (uses customers-table))
(declare (uses hash))
(declare (uses list))
(declare (uses shipping-addresses-table))
(declare (uses validation))
(declare (uses validation-service-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request new-customer-request
  (first-name string #t 1 50)
  (last-name string #t 1 50)
  (birthdate date #t)
  (shipping-addresses list #t 1 10 (shipping-address new-customer-shipping-address-subrequest #t)))

(define-request new-customer-shipping-address-subrequest
  (street string #t 1 100)
  (city string #t 1 50)
  (state string #t 1 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response new-customer-response
  (customer-id integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (new-customer-service sql-connection new-customer-request)

  ;; validate the request
  (validate-request new-customer-request validate-new-customer-request)

  ;; insert a customer-row
  (let ((customer-id
          (customers-table-insert
            sql-connection
            (make-customer-row
              0
              (new-customer-request-first-name new-customer-request)
              (new-customer-request-last-name new-customer-request)
              (new-customer-request-birthdate new-customer-request)))))

    ;; insert the shipping-address-rows
    (for-each
      (lambda (new-customer-shipping-address-subrequest)
        (shipping-addresses-table-insert
          sql-connection
          (make-shipping-address-row
            0
            customer-id
            (new-customer-shipping-address-subrequest-street new-customer-shipping-address-subrequest)
            (new-customer-shipping-address-subrequest-city new-customer-shipping-address-subrequest)
            (new-customer-shipping-address-subrequest-state new-customer-shipping-address-subrequest))))

      (new-customer-request-shipping-addresses new-customer-request))

    ;; make the new-customer-response
    (make-new-customer-response
      customer-id)))
