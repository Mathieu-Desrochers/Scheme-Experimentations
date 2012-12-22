
(use srfi-1)

(declare (unit request-validation))
(declare (uses validation))

;; validates a request value
(define
  (validate-request-value
    value
    validation-procedure
    validation-parameters
    invalid-value-symbol)
    (if (not (apply validation-procedure value validation-parameters))
      (list (cons invalid-value-symbol value))
      '()))

;; validates a request integer
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

;; validates a request number
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

;; validates a request string
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
    invalid-length-symbol
    element-validation-procedure)
    (if (not (validate-list value required min-length max-length))
      (if (not (list? value))
        (list (cons invalid-value-symbol value))
        (list (cons invalid-length-symbol (length value))))
      (if value
        (concatenate
          (map
            element-validation-procedure
            value))
        '())))

;; validates a request integer list
(define
  (validate-request-integer-list
    value
    required
    min-length
    max-length
    invalid-value-symbol
    invalid-length-symbol
    element-min-value
    element-max-value
    invalid-element-value-symbol)
    (validate-request-list
      value
      required
      min-length
      max-length
      invalid-value-symbol
      invalid-length-symbol
      (lambda (element-value)
        (validate-request-integer
          element-value
          #t
          element-min-value
          element-max-value
          invalid-element-value-symbol))))

;; validates a request number list
(define
  (validate-request-number-list
    value
    required
    min-length
    max-length
    invalid-value-symbol
    invalid-length-symbol
    element-min-value
    element-max-value
    invalid-element-value-symbol)
    (validate-request-list
      value
      required
      min-length
      max-length
      invalid-value-symbol
      invalid-length-symbol
      (lambda (element-value)
        (validate-request-number
          element-value
          #t
          element-min-value
          element-max-value
          invalid-element-value-symbol))))

;; validates a request string list
(define
  (validate-request-string-list
    value
    required
    min-length
    max-length
    invalid-value-symbol
    invalid-length-symbol
    element-min-length
    element-max-length
    invalid-element-value-symbol)
    (validate-request-list
      value
      required
      min-length
      max-length
      invalid-value-symbol
      invalid-length-symbol
      (lambda (element-value)
        (validate-request-string
          element-value
          #t
          element-min-length
          element-max-length
          invalid-element-value-symbol))))

;; validates a subrequest
(define
  (validate-subrequest
    value
    required
    record-validation-procedure
    subrequest-validation-procedure
    invalid-value-symbol)
    (if (not (validate-record value required record-validation-procedure))
      (list (cons invalid-value-symbol value))
      (if value
        (subrequest-validation-procedure value)
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
    record-validation-procedure
    subrequest-validation-procedure
    invalid-element-value-symbol)
    (validate-request-list
      value
      required
      min-length
      max-length
      invalid-value-symbol
      invalid-length-symbol
      (lambda (element-value)
        (validate-subrequest
          element-value
          #t
          record-validation-procedure
          subrequest-validation-procedure
          invalid-element-value-symbol))))
