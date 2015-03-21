
(use srfi-1)
(use srfi-69)

(declare (unit update-customer-service))

(declare (uses compare))
(declare (uses customers-table))
(declare (uses date-time))
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
  (birthdate date #t))

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
        (update-customer-request-birthdate update-customer-request)))

    ;; make the update-customer-response
    (make-update-customer-response)))
