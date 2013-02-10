
(declare (unit new-customer-service))
(declare (uses customer-addresses-table))
(declare (uses customers-table))
(declare (uses sql))

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
;; service definition

;(define-service new-customer-service
;  "POST"
;  "/new-customer")

;(declare (uses http))

;; returns the service registration
(define (new-customer-service-registration)
  (make-http-service-registration
    "POST"
    "/new-customer"
    new-customer-request-parse
    new-customer-service
    new-customer-response-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (new-customer-service sql-connection new-customer-request)
  (define (make-customer-address-rows customer-id)
    (map
      (lambda (new-customer-address-subrequest)
        (let ((address (new-customer-address-subrequest-address new-customer-address-subrequest))
              (city (new-customer-address-subrequest-city new-customer-address-subrequest))
              (state (new-customer-address-subrequest-state new-customer-address-subrequest)))
          (make-customer-address-row 0 customer-id address city state)))
      (new-customer-request-addresses new-customer-request)))
  (let* ((first-name (new-customer-request-first-name new-customer-request))
         (last-name (new-customer-request-last-name new-customer-request))
         (customer-row (make-customer-row 0 first-name last-name))
         (customer-id (customers-table-insert sql-connection customer-row))
         (customer-address-rows (make-customer-address-rows customer-id)))
    (map
      (lambda (customer-address-row)
        (customer-addresses-table-insert sql-connection customer-address-row))
      customer-address-rows)
    (make-new-customer-response customer-id)))
