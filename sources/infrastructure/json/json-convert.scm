
(declare (unit json-convert))

(declare (uses datetime))

;; upgrades a value from a field type
;; which cannot be represented in json
(define (json-upgrade-value value field-type)
  (define (json-upgrade-from-string upgrade-procedure)
    (if (not (string? value)) value
      (let ((upgraded-value (upgrade-procedure value)))
        (if upgraded-value upgraded-value value))))
  (cond ((eq? field-type 'date) (json-upgrade-from-string string->date))
        ((eq? field-type 'datetime) (json-upgrade-from-string string->datetime))
        ((eq? field-type 'time) (json-upgrade-from-string string->time))
        (else value)))

;; downgrades a value to a field type
;; which can be represented in json
(define (json-downgrade-value value)
  (cond ((date? value) (date->string value))
        ((datetime? value) (datetime->string value))
        ((time? value) (time->string value))
        (else value)))
