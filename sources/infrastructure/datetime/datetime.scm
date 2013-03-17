
(declare (unit datetime))

(declare (uses datetime-intern))
(declare (uses scdtl))

;; encapsulates a date
(define-record date year month day)

;; encapsulates a datetime
(define-record datetime year month day hour minute second)

;; encapsulates a time
(define-record time hour minute second)

;; parses a string representing a date
(define (string->date string)
  (datetime-with-parsed-scdtl-tm* string "%Y-%m-%d"
    (lambda (scdtl-tm*)
      (make-date
        (+ (scdtl-tm-year scdtl-tm*) 1900)
        (+ (scdtl-tm-mon scdtl-tm*) 1)
        (scdtl-tm-mday scdtl-tm*)))))

;; parses a string representing a datetime
(define (string->datetime string)
  (datetime-with-parsed-scdtl-tm* string "%Y-%m-%dT%H:%M:%S"
    (lambda (scdtl-tm*)
      (make-datetime
        (+ (scdtl-tm-year scdtl-tm*) 1900)
        (+ (scdtl-tm-mon scdtl-tm*) 1)
        (scdtl-tm-mday scdtl-tm*)
        (scdtl-tm-hour scdtl-tm*)
        (scdtl-tm-min scdtl-tm*)
        (scdtl-tm-sec scdtl-tm*)))))

;; parses a string representing a time
(define (string->time string)
  (datetime-with-parsed-scdtl-tm* string "%H:%M:%S"
    (lambda (scdtl-tm*)
      (make-time
        (scdtl-tm-hour scdtl-tm*)
        (scdtl-tm-min scdtl-tm*)
        (scdtl-tm-sec scdtl-tm*)))))

;; serializes a date to string
(define (date->string date)
  (datetime-serialize-scdtl-tm
    (date-year date)
    (date-month date)
    (date-day date)
    0
    0
    0
    "%Y-%m-%d"))

;; serializes a datetime to string
(define (datetime->string datetime)
  (datetime-serialize-scdtl-tm
    (datetime-year datetime)
    (datetime-month datetime)
    (datetime-day datetime)
    (datetime-hour datetime)
    (datetime-minute datetime)
    (datetime-second datetime)
    "%Y-%m-%dT%H:%M:%S"))

;; serializes a time to string
(define (time->string time)
  (datetime-serialize-scdtl-tm
    0
    0
    0
    (time-hour time)
    (time-minute time)
    (time-second time)
    "%H:%M:%S"))
