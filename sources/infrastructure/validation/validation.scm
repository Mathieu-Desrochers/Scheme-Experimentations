
(declare (unit validation))

;; validates an integer
(define (validate-integer value required min-value max-value)
  (cond ((not value) (not required))
        ((not (and (integer? value) (exact? value))) #f)
        ((< value min-value) #f)
        ((> value max-value) #f)
        (else #t)))

;; validates a number
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

;; validates a record
(define (validate-record value required record-type-validation-procedure)
  (cond ((not value) (not required))
        ((not (record-type-validation-procedure value)) #f)
        (else #t)))

;; validates a value
(define
  (validate-value
    value
    validation-procedure
    validation-parameters
    invalid-value-symbol)
    (if (not (apply validation-procedure value validation-parameters))
      (list (cons invalid-value-symbol value))
      '()))

;; validates a value list
(define
  (validate-value-list
    value
    required
    min-length
    max-length
    invalid-value-symbol
    invalid-length-symbol
    element-validation-procedure
    element-validation-parameters
    invalid-element-value-symbol)
    (if (not (validate-list value required min-length max-length))
      (if (not (list? value))
        (list (cons invalid-value-symbol value))
        (list (cons invalid-length-symbol (length value))))
      (if value
        (concatenate
          (map
            (lambda (element-value)
              (validate-value
                element-value
                element-validation-procedure
                element-validation-parameters
                invalid-element-value-symbol))
            value))
        '())))

;; validates a subrequest
(define
  (validate-subrequest
    value
    required
    record-type-validation-procedure
    record-validation-procedure
    invalid-value-symbol)
    (if (not (validate-record value required record-type-validation-procedure))
      (list (cons invalid-value-symbol value))
      (if value
        (record-validation-procedure value)
        '())))

;; validates a subrequest list
(define
  (validate-subrequest-list
    value
    required
    min-length
    max-length
    invalid-value-symbol
    invalid-length-symbol
    element-required
    element-record-type-validation-procedure
    element-record-validation-procedure
    invalid-element-value-symbol)
    (if (not (validate-list value required min-length max-length))
      (if (not (list? value))
        (list (cons invalid-value-symbol value))
        (list (cons invalid-length-symbol (length value))))
      (if value
        (concatenate
          (map
            (lambda (element-value)
              (validate-subrequest
                element-value
                element-required
                element-record-type-validation-procedure
                element-record-validation-procedure
                invalid-element-value-symbol))
            value))
        '())))
