
(use srfi-1)

(declare (unit request-validation))
(declare (uses validation))

; validates a request field
(define (validate-request-field value validation-procedure-and-parameters invalid-value-symbol)
  (let ((validation-procedure (car validation-procedure-and-parameters))
        (validation-parameters (cdr validation-procedure-and-parameters)))
    (if (not (apply validation-procedure value validation-parameters))
      (list (cons invalid-value-symbol value))
      '())))

; validates a request integer field
(define (validate-request-integer-field value required min-value max-value invalid-value-symbol)
  (validate-request-field value (list validate-integer required min-value max-value) invalid-value-symbol))

; validates a request number field
(define (validate-request-number-field value required min-value max-value invalid-value-symbol)
  (validate-request-field value (list validate-number required min-value max-value) invalid-value-symbol))

; validates a request string field
(define (validate-request-string-field value required min-length max-length invalid-value-symbol)
  (validate-request-field value (list validate-string required min-length max-length) invalid-value-symbol))

;; validates a request field list
(define (validate-request-field-list value required min-length max-length invalid-value-symbol element-validation-procedure . element-validation-parameters)
  (if (not (validate-list value required min-length max-length))
    (list (cons invalid-value-symbol value))
    (if value
      (concatenate
        (map
          (lambda (element-value)
            (apply element-validation-procedure element-value element-validation-parameters))
          value))
      '())))

; validates a request integer field list
(define (validate-request-integer-field-list value required min-length max-length invalid-value-symbol element-required element-min-value element-max-value invalid-element-value-symbol)
  (validate-request-field-list value required min-length max-length invalid-value-symbol validate-request-integer-field element-required element-min-value element-max-value invalid-element-value-symbol))

; validates a request number field list
(define (validate-request-number-field-list value required min-length max-length invalid-value-symbol element-required element-min-value element-max-value invalid-element-value-symbol)
  (validate-request-field-list value required min-length max-length invalid-value-symbol validate-request-number-field element-required element-min-value element-max-value invalid-element-value-symbol))

; validates a request string field list
(define (validate-request-string-field-list value required min-length max-length invalid-value-symbol element-required element-min-length element-max-length invalid-element-value-symbol)
  (validate-request-field-list value required min-length max-length invalid-value-symbol validate-request-string-field element-required element-min-length element-max-length invalid-element-value-symbol))
