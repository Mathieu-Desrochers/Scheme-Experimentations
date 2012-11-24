
(declare (unit validation))

; validates an integer
(define (validate-integer integer)
  (cond ((not (integer? integer)) #f)
        (else #t)))

;; validates a string
(define (validate-string string)
  (cond ((not (string? string)) #f)
        (else #t)))

; validates a list
(define (validate-list list)
  (cond ((not (list? list)) #f)
        (else #t)))
