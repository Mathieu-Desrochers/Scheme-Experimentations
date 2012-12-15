
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wishful macro invocation

(define-request new-customer-request
  (birthdate request new-customer-birthdate-subrequest))

(define-request new-customer-birthdate-subrequest
  (years list #t 1 5 year integer #t 1000 5000)
  (month integer #t 1 12)
  (day integer #t 1 31))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(define new-customer-birthdate-subrequest (make-new-customer-birthdate-subrequest '(2000 2012) 1 1))
(define new-customer-request (make-new-customer-request new-customer-birthdate-subrequest))
(validate-new-customer-request new-customer-request)
