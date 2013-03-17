
(declare (unit new-customer-service))

(declare (uses customers-table))
(declare (uses shipping-addresses-table))
(declare (uses sql))
(declare (uses validation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request new-customer-request
  (first-name string #t 1 50)
  (last-name string #t 1 50)
  (is-vip boolean)
  (shipping-address new-customer-shipping-address-subrequest #t))

(define-request new-customer-shipping-address-subrequest
  (street string #t 1 100)
  (city string #t 1 50)
  (state string #t 1 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response new-customer-response
  (customer-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (new-customer-service sql-connection new-customer-request)

  ;; validate the request
  (let ((validation-errors (validate-new-customer-request new-customer-request)))
    (when (not (null? validation-errors))
      (abort-validation-errors validation-errors)))

  ;; insert a customer-row
  (let ((customer-id
          (customers-table-insert
            sql-connection
            (make-customer-row 0
              (new-customer-request-first-name new-customer-request)
              (new-customer-request-last-name new-customer-request)
              (new-customer-request-is-vip new-customer-request)))))

    ;; insert a shipping-address-row
    (let ((shipping-address-subrequest (new-customer-request-shipping-address new-customer-request)))
      (shipping-addresses-table-insert
        sql-connection
        (make-shipping-address-row 0
          customer-id
          "2001-01-01"
          (new-customer-shipping-address-subrequest-street shipping-address-subrequest)
          (new-customer-shipping-address-subrequest-city shipping-address-subrequest)
          (new-customer-shipping-address-subrequest-state shipping-address-subrequest))))

    ;; make the service response
    (make-new-customer-response customer-id)))
