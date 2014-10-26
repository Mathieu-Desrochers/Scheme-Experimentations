
(use srfi-1)

(declare (unit validation-service-request))

(declare (uses validation))

;; maps a validation procedure to the elements of a list
(define (map-validation-procedure value element-symbol validation-procedure)
  (concatenate
    (map
      (lambda (index)
        (let* ((index-symbol (string->symbol (number->string index)))
               (element-symbol-indexed (symbol-append element-symbol index-symbol))
               (element-value (list-ref value index)))
          (validation-procedure element-symbol-indexed element-value)))
      (iota (length value)))))

;; validates a list and its elements
(define
  (validate-list-and-elements
    field-prefix
    field-symbol
    field-value
    field-required
    field-min-length
    field-max-length
    element-field-symbol
    element-field-validation-procedure)
  (let ((validation-error
          (validate-list
            field-value
            field-required
            field-min-length
            field-max-length)))
    (if validation-error
      (list (symbol-append field-prefix field-symbol '- validation-error))
      (if field-value
        (map-validation-procedure
          field-value
          element-field-symbol
          element-field-validation-procedure)
        '()))))

;; validates a value
(define
  (validate-value
    field-prefix
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
      (list (symbol-append field-prefix field-symbol '- validation-error))
      '())))

;; validates a value list
(define
  (validate-value-list
    field-prefix
    field-symbol
    field-value
    field-required
    field-min-length
    field-max-length
    element-field-symbol
    element-field-validation-procedure
    element-field-validation-parameters)
  (validate-list-and-elements
    field-prefix
    field-symbol
    field-value
    field-required
    field-min-length
    field-max-length
    element-field-symbol
    (lambda (element-field-symbol-indexed element-field-value)
      (validate-value
        field-prefix
        element-field-symbol-indexed
        element-field-value
        element-field-validation-procedure
        element-field-validation-parameters))))

;; validates a subrequest
(define
  (validate-subrequest
    field-prefix
    field-symbol
    field-value
    field-required
    field-type-validation-procedure
    field-validation-procedure)
  (let ((validation-error
          (validate-record
            field-value
            field-required
            field-type-validation-procedure)))
    (if validation-error
      (list (symbol-append field-prefix field-symbol '- validation-error))
      (if field-value
        (let ((nested-field-prefix (symbol-append field-prefix field-symbol '-)))
          (field-validation-procedure field-value nested-field-prefix))
        '()))))

;; validates a subrequest list
(define
  (validate-subrequest-list
    field-prefix
    field-symbol
    field-value
    field-required
    field-min-length
    field-max-length
    element-field-symbol
    element-field-required
    element-field-type-validation-procedure
    element-field-validation-procedure)
  (validate-list-and-elements
    field-prefix
    field-symbol
    field-value
    field-required
    field-min-length
    field-max-length
    element-field-symbol
    (lambda (element-field-symbol-indexed element-field-value)
      (validate-subrequest
        field-prefix
        element-field-symbol-indexed
        element-field-value
        element-field-required
        element-field-type-validation-procedure
        element-field-validation-procedure))))

;; validates a request
(define (validate-request request validate-request-procedure)
  (let ((validation-errors (validate-request-procedure request)))
    (when (not (null? validation-errors))
      (abort-validation-errors validation-errors))))
