
(declare (unit validation))

; validates an integer
(define (validate-integer value required min-value max-value)
  (cond ((not value) (not required))
        ((not (integer? value)) #f)
        ((< value min-value) #f)
        ((> value max-value) #f)
        (else #t)))

; validates a number
(define (validate-number value required min-value max-value)
  (cond ((not value) (not required))
        ((not (number? value)) #f)
        ((< value min-value) #f)
        ((> value max-value) #f)
        (else #t)))

;; validates a string
(define (validate-string value required min-length max-length)
  (cond ((not value) (not required))
        ((not (string? value)) #f)
        ((< (string-length value) min-length) #f)
        ((> (string-length value) max-length) #f)
        (else #t)))

;; validates a list
(define (validate-list value required min-length max-length)
  (cond ((not value) (not required))
        ((not (list? value)) #f)
        ((< (length value) min-length) #f)
        ((> (length value) max-length) #f)
        (else #t)))
