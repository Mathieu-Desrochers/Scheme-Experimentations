
(declare (unit json-convert))

(declare (uses date-time))

;; upgrades a value from a field type
;; which cannot be represented in json
(define (json-upgrade-value value field-type)
  (define (json-upgrade-from-string upgrade-procedure)
    (if (not (string? value)) value
      (let ((upgraded-value (upgrade-procedure value)))
        (if upgraded-value upgraded-value value))))
  (cond ((eq? field-type 'date) (json-upgrade-from-string string->date))
        ((eq? field-type 'date-time) (json-upgrade-from-string string->date-time))
        ((eq? field-type 'date-time-without-seconds) (json-upgrade-from-string string->date-time-without-seconds))
        ((eq? field-type 'day-of-week) (json-upgrade-from-string string->day-of-week))
        ((eq? field-type 'time) (json-upgrade-from-string string->time))
        ((eq? field-type 'time-without-seconds) (json-upgrade-from-string string->time-without-seconds))
        (else value)))

;; downgrades a value to a field type
;; which can be represented in json
(define (json-downgrade-value value)
  (cond ((date? value) (date->string value))
        ((date-time? value) (date-time->string value))
        ((date-time-without-seconds? value) (date-time-without-seconds->string value))
        ((day-of-week? value) (day-of-week->string value))
        ((time? value) (time->string value))
        ((time-without-seconds? value) (time-without-seconds->string value))
        (else value)))
