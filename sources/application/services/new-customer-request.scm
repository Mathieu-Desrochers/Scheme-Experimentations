
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wishful macro invocation

(define-request new-customer-request
  (first-name string)
  (last-name string)
  (addresses list new-customer-request-address)
  (birthdate new-customer-request-birthdate)
  (numbers list integer))

(define-request new-customer-request-address
  (address string)
  (city string)
  (state string))

(define-request new-customer-request-birthdate
  (year integer)
  (month integer)
  (day integer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(define new-customer-request-birthdate (make-new-customer-request-birthdate 2000 "a" "b"))
(validate-new-customer-request-birthdate new-customer-request-birthdate)