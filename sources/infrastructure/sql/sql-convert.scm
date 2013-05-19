
(declare (unit sql-convert))

(declare (uses datetime))

;; upgrades a value from a column type
;; which cannot be represented in sqlite3
(define (sql-upgrade-value value column-type)
  (cond ((eq? column-type 'boolean) (eq? value 1))
        ((eq? column-type 'date) (string->date value))
        ((eq? column-type 'datetime) (string->datetime value))
        ((eq? column-type 'day-of-week) (integer->day-of-week value))
        ((eq? column-type 'time) (string->time value))
        (else value)))

;; downgrades a value to a column type
;; which can be represented in sqlite3
(define (sql-downgrade-value value)
  (cond ((boolean? value) (if value 1 0))
        ((date? value) (date->string value))
        ((datetime? value) (datetime->string value))
        ((day-of-week? value) (day-of-week->integer value))
        ((time? value) (time->string value))
        (else value)))
