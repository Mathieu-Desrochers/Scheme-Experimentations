
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wishful macro invocation

(define-request new-customer-request
  (first-name string)
  (last-name string)
  (addresses list new-customer-request-address)
  (birthdate new-customer-request-birthdate)
  (numbers list integer))

(define-request new-customer-request-address
  (address string #t 1 100)
  (city string #t 1 100)
  (state string #t 1 100))

(define-request new-customer-request-birthdate
  (year integer #t 1000 5000)
  (month integer #t 1 12)
  (day integer #t 1 31))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(define new-customer-request-birthdate (make-new-customer-request-birthdate 2000 1 1))
(validate-new-customer-request-birthdate new-customer-request-birthdate)