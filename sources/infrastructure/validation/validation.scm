
(use srfi-1)

(declare (unit validation))

;; validates an integer
(define (validate-integer value required min-value max-value)
  (cond ((not value) (if (not required) #f 'missing))
        ((not (and (integer? value) (exact? value))) 'wrong-type)
        ((< value min-value) 'too-low)
        ((> value max-value) 'too-high)
        (else #f)))

;; validates a number
(define (validate-number value required min-value max-value)
  (cond ((not value) (if (not required) #f 'missing))
        ((not (number? value)) 'wrong-type)
        ((< value min-value) 'too-low)
        ((> value max-value) 'too-high)
        (else #f)))

;; validates a string
(define (validate-string value required min-length max-length)
  (cond ((not value) (if (not required) #f 'missing))
        ((not (string? value)) 'wrong-type)
        ((< (string-length value) min-length) 'too-short)
        ((> (string-length value) max-length) 'too-long)
        (else #f)))

;; validates a list
(define (validate-list value required min-length max-length)
  (cond ((not value) (if (not required) #f 'missing))
        ((not (list? value)) 'wrong-type)
        ((< (length value) min-length) 'too-few)
        ((> (length value) max-length) 'too-many)
        (else #f)))

;; validates a record
(define (validate-record value required record-type-validation-procedure)
  (cond ((not value) (if (not required) #f 'missing))
        ((not (record-type-validation-procedure value)) 'wrong-type)
        (else #f)))

;; validates a value
(define
  (validate-value
    field-symbol
    field-value
    field-validation-procedure
    field-validation-parameters)
  (let ((validation-error
          (apply
            field-validation-procedure
            field-value
            field-validation-parameters)))
    (if validation-error
      (list (symbol-append field-symbol '- validation-error))
      '())))

;; validates a value list
(define
  (validate-value-list
    field-symbol
    field-value
    field-required
    field-min-length
    field-max-length
    element-field-symbol
    element-validation-procedure
    element-validation-parameters)
  (let ((validation-error
          (validate-list
            field-value
            field-required
            field-min-length
            field-max-length)))
    (if validation-error
      (list (symbol-append field-symbol '- validation-error))
      (if field-value
        (concatenate
          (map
            (lambda (index)
              (let* ((index-symbol
                      (string->symbol (number->string index)))
                     (element-field-symbol-indexed
                      (symbol-append element-field-symbol index-symbol))
                     (element-field-value
                      (list-ref field-value index)))
                (validate-value
                  element-field-symbol-indexed
                  element-field-value
                  element-validation-procedure
                  element-validation-parameters)))
            (iota (length field-value))))
        '()))))

;; validates a subrequest
(define
  (validate-subrequest
    field-symbol
    field-value
    field-required
    subrequest-type-validation-procedure
    subrequest-validation-procedure)
  (let ((validation-error
          (apply
            validate-record
            (list
              field-value
              field-required
              subrequest-type-validation-procedure))))
    (if validation-error
      (symbol-append field-symbol '- validation-error)
      (if field-value
        (subrequest-validation-procedure field-value)
        #f))))

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
