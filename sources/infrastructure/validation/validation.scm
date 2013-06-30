
(use srfi-1)

(declare (unit validation))

(declare (uses date-time))

;; validates a boolean
(define (validate-boolean value)
  (cond ((not (boolean? value)) 'wrong-type)
        (else #f)))

;; validates a date
(define (validate-date value required)
  (cond ((not value) (if (not required) #f 'missing))
        ((not (date? value)) 'wrong-type)
        ((not (date-valid? value)) 'invalid)
        (else #f)))

;; validates a date time
(define (validate-date-time value required)
  (cond ((not value) (if (not required) #f 'missing))
        ((not (date-time? value)) 'wrong-type)
        ((not (date-time-valid? value)) 'invalid)
        (else #f)))

;; validates a day of week
(define (validate-day-of-week value required)
  (cond ((not value) (if (not required) #f 'missing))
        ((not (day-of-week? value)) 'wrong-type)
        ((not (day-of-week-valid? value)) 'invalid)
        (else #f)))

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

;; validates a time
(define (validate-time value required)
  (cond ((not value) (if (not required) #f 'missing))
        ((not (time? value)) 'wrong-type)
        ((not (time-valid? value)) 'invalid)
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

;; raises an exception caused by a validation error
(define (abort-validation-error validation-error)
  (abort-validation-errors (list validation-error)))

;; raises an exception caused by validation errors
(define (abort-validation-errors validation-errors)
  (let ((condition (make-property-condition 'validation 'errors validation-errors)))
    (abort condition)))

;; makes a list of numbered validation errors
(define (make-numbered-validation-errors validation-error-prefix numbers validation-error-suffix)
  (map
    (lambda (number)
      (symbol-append
        validation-error-prefix
        (string->symbol (number->string number))
        validation-error-suffix))
    numbers))

;; returns whether an exception was caused by validation errors
(define (validation-exception? exception)
  ((condition-predicate 'validation) exception))

;; returns the validation errors from an exception
(define (validation-errors exception)
  ((condition-property-accessor 'validation 'errors) exception))
