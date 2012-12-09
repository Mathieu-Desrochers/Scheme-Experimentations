
(use srfi-1)

(declare (unit request-validation))
(declare (uses validation))

; validates a request value
(define
  (validate-request-value
    value
    validation-procedure
    validation-parameters
    invalid-value-symbol)
    (if (not (apply validation-procedure value validation-parameters))
      (list (cons invalid-value-symbol value))
      '()))

; validates a request integer
(define
  (validate-request-integer
    value
    required
    min-value
    max-value
    invalid-value-symbol)
    (validate-request-value
      value
      validate-integer
      (list required min-value max-value)
      invalid-value-symbol))

; validates a request number
(define
  (validate-request-number
    value
    required
    min-value
    max-value
    invalid-value-symbol)
    (validate-request-value
      value
      validate-number
      (list required min-value max-value)
      invalid-value-symbol))

; validates a request string
(define
  (validate-request-string
    value
    required
    min-length
    max-length
    invalid-value-symbol)
    (validate-request-value
      value
      validate-string
      (list required min-length max-length)
      invalid-value-symbol))

;; validates a request list
(define
  (validate-request-list
    value
    required
    min-length
    max-length
    invalid-value-symbol
    element-validation-procedure)
    (if (not (validate-list value required min-length max-length))
      (list (cons invalid-value-symbol value))
      (if value
        (concatenate
          (map
            element-validation-procedure
            value))
        '())))

; validates a request integer list
(define
  (validate-request-integer-list
    value
    required
    min-length
    max-length
    invalid-value-symbol
    element-required
    element-min-value
    element-max-value
    invalid-element-value-symbol)
    (validate-request-list
      value
      required
      min-length
      max-length
      invalid-value-symbol
      (lambda (element-value)
        (validate-request-integer
          element-value
          element-required
          element-min-value
          element-max-value
          invalid-element-value-symbol))))

; validates a request number list
(define
  (validate-request-number-list
    value
    required
    min-length
    max-length
    invalid-value-symbol
    element-required
    element-min-value
    element-max-value
    invalid-element-value-symbol)
    (validate-request-list
      value
      required
      min-length
      max-length
      invalid-value-symbol
      (lambda (element-value)
        (validate-request-number
          element-value
          element-required
          element-min-value
          element-max-value
          invalid-element-value-symbol))))

; validates a request string list
(define
  (validate-request-string-list
    value
    required
    min-length
    max-length
    invalid-value-symbol
    element-required
    element-min-length
    element-max-length
    invalid-element-value-symbol)
    (validate-request-list
      value
      required
      min-length
      max-length
      invalid-value-symbol
      (lambda (element-value)
        (validate-request-string
          element-value
          element-required
          element-min-length
          element-max-length
          invalid-element-value-symbol))))
