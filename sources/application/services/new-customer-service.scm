
(declare (unit new-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request new-customer-request
  (first-name string #t 1 100)
  (last-name string #t 1 100)
  (adresses list #t 1 5 (address new-customer-address-subrequest #t)))

(define-request new-customer-address-subrequest
  (address string #t 1 100)
  (city string #t 1 100)
  (state string #t 1 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response get-customer-response
  (name)
  (credit-score)
  (credit-limit))
