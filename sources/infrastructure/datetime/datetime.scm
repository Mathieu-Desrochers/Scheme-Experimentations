
(declare (unit datetime))

(declare (uses datetime-intern))
(declare (uses scdtl))

;; encapsulates a date
(define-record date year month day)

;; encapsulates a datetime expressed in UTC
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
  (datetime-with-parsed-scdtl-tm* string "%Y-%m-%dT%H:%M:%SZ"
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
  (format-datetime
    (date-year date)
    (date-month date)
    (date-day date)
    0
    0
    0
    "%Y-%m-%d"))

;; serializes a datetime to string
(define (datetime->string datetime)
  (format-datetime
    (datetime-year datetime)
    (datetime-month datetime)
    (datetime-day datetime)
    (datetime-hour datetime)
    (datetime-minute datetime)
    (datetime-second datetime)
    "%Y-%m-%dT%H:%M:%SZ"))

;; serializes a time to string
(define (time->string time)
  (format-datetime
    0
    0
    0
    (time-hour time)
    (time-minute time)
    (time-second time)
    "%H:%M:%S"))

;; returns whether a date is valid
(define (date-valid? date)
  (with-normalized-datetime
    (date-year date)
    (date-month date)
    (date-day date)
    0
    0
    0
    (lambda (year month day hour minute second)
      (let ((normalized-date (make-date year month day)))
        (and (equal? date normalized-date)
             (> year 1000))))))

;; returns whether a datetime is valid
(define (datetime-valid? datetime)
  (with-normalized-datetime
    (datetime-year datetime)
    (datetime-month datetime)
    (datetime-day datetime)
    (datetime-hour datetime)
    (datetime-minute datetime)
    (datetime-second datetime)
    (lambda (year month day hour minute second)
      (let ((normalized-datetime (make-datetime year month day hour minute second)))
        (and (equal? datetime normalized-datetime)
             (> year 1000))))))

;; returns whether a time is valid
(define (time-valid? time)
  (with-normalized-datetime
    0
    0
    0
    (time-hour time)
    (time-minute time)
    (time-second time)
    (lambda (year month day hour minute second)
      (let ((normalized-time (make-time hour minute second)))
        (equal? time normalized-time)))))

;; returns the current date
(define (date-now)
  (with-datetime-now
    (lambda (year month day hour minute second)
      (make-date year month day))))

;; returns the current datetime
(define (datetime-now)
  (with-datetime-now
    (lambda (year month day hour minute second)
      (make-datetime year month day hour minute second))))

;; returns the current time
(define (time-now)
  (with-datetime-now
    (lambda (year month day hour minute second)
      (make-time hour minute second))))
