
(declare (unit new-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request new-customer-request
  (first-name string #t 1 100)
  (last-name string #t 1 100)
  (adresses list request #t 1 5 address new-customer-request-address-subrequest))

(define-request new-customer-request-address-subrequest
  (address string #t 1 100)
  (city string #t 1 100)
  (state string #t 1 100))
