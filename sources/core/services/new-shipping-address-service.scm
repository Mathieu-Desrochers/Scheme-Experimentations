
(declare (unit new-shipping-address-service))

(declare (uses customers-table))
(declare (uses datetime))
(declare (uses shipping-addresses-table))
(declare (uses sql))
(declare (uses validation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request new-shipping-address-request
  (customer new-shipping-address-customer-subrequest #t)
  (shipping-address new-shipping-address-shipping-address-subrequest #t))

(define-request new-shipping-address-customer-subrequest
  (customer-id integer #t 1 1000000000))

(define-request new-shipping-address-shipping-address-subrequest
  (street string #t 1 100)
  (city string #t 1 50)
  (state string #t 1 50)
  (effective-date date #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response new-shipping-address-response
  (shipping-address-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (new-shipping-address-service sql-connection new-shipping-address-request)

  ;; validate the request
  (let ((validation-errors (validate-new-shipping-address-request new-shipping-address-request)))
    (when (not (null? validation-errors))
      (abort-validation-errors validation-errors)))

  ;; validate the customer-id
  (let* ((customer-subrequest (new-shipping-address-request-customer new-shipping-address-request))
         (customer-id (new-shipping-address-customer-subrequest-customer-id customer-subrequest))
         (customer-row (customers-table-select-by-customer-id customer-id)))
    (when (null? customer-row)
      (abort-validation-error 'customer-id-unknown))

    ;; insert a shipping-address-row
    (let* ((shipping-address-subrequest (new-shipping-address-request-shipping-address new-shipping-address-request))
           (shipping-address-id
            (shipping-addresses-table-insert
              sql-connection
              (make-shipping-address-row 0
                customer-id
                (new-shipping-address-shipping-address-subrequest-effective-date shipping-address-subrequest)
                (new-shipping-address-shipping-address-subrequest-street shipping-address-subrequest)
                (new-shipping-address-shipping-address-subrequest-city shipping-address-subrequest)
                (new-shipping-address-shipping-address-subrequest-state shipping-address-subrequest)))))

      ;; make the service response
      (make-new-shipping-address-response shipping-address-id))))
