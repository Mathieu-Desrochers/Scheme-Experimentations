
(declare (unit sql-convert))

(declare (uses date-time))

;; upgrades a value from a column type
;; which cannot be represented in sqlite3
(define (sql-upgrade-value value column-type)
  (cond ((eq? column-type 'boolean) (eq? value 1))
        ((eq? column-type 'date) (string->date value))
        ((eq? column-type 'date-time) (string->date-time value))
        ((eq? column-type 'date-time-without-seconds) (string->date-time-without-seconds value))
        ((eq? column-type 'day-of-week) (integer->day-of-week value))
        ((eq? column-type 'time) (string->time value))
        (else value)))

;; downgrades a value to a column type
;; which can be represented in sqlite3
(define (sql-downgrade-value value)
  (cond ((boolean? value) (if value 1 0))
        ((date? value) (date->string value))
        ((date-time? value) (date-time->string value))
        ((date-time-without-seconds? value) (date-time-without-seconds->string value))
        ((day-of-week? value) (day-of-week->integer value))
        ((time? value) (time->string value))
        (else value)))
