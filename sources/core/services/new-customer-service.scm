
(declare (unit new-customer-service))

(declare (uses customer-addresses-table))
(declare (uses customers-table))
(declare (uses sql))
(declare (uses validation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request new-customer-request
  (first-name string #t 1 100)
  (last-name string #t 1 100)
  (addresses list #t 1 5 (address new-customer-address-subrequest #t)))

(define-request new-customer-address-subrequest
  (address string #t 1 100)
  (city string #t 1 100)
  (state string #t 1 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response new-customer-response
  (customer-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (new-customer-service sql-connection new-customer-request)

  ;; makes a customer-address-row
  ;; for every new-customer-address-subrequest
  (define (make-customer-address-rows customer-id)
    (map
      (lambda (new-customer-address-subrequest)
        (let ((address (new-customer-address-subrequest-address new-customer-address-subrequest))
              (city (new-customer-address-subrequest-city new-customer-address-subrequest))
              (state (new-customer-address-subrequest-state new-customer-address-subrequest)))
          (make-customer-address-row 0 customer-id address city state)))
      (new-customer-request-addresses new-customer-request)))

  ;; validate the request
  (let ((validation-errors (validate-new-customer-request new-customer-request)))
    (when (not (null? validation-errors))
      (abort-validation-errors validation-errors)))

  ;; insert a customer-row
  (let* ((first-name (new-customer-request-first-name new-customer-request))
         (last-name (new-customer-request-last-name new-customer-request))
         (customer-row (make-customer-row 0 first-name last-name))
         (customer-id (customers-table-insert sql-connection customer-row)))

    ;; insert the customer-address-rows
    (let ((customer-address-rows (make-customer-address-rows customer-id)))
      (map
        (lambda (customer-address-row)
          (customer-addresses-table-insert sql-connection customer-address-row))
        customer-address-rows)

      ;; make the service response
      (make-new-customer-response customer-id))))
