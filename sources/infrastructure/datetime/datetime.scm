
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

;; adds years to a date
(define (date-add-years date years)
  (make-date
    (+ (date-year date) years)
    (date-month date)
    (date-day date)))

;; adds months to a date
(define (date-add-months date months)
  (with-normalized-datetime
    (date-year date)
    (+ (date-month date) months)
    (date-day date)
    0
    0
    0
    (lambda (year month day hour minute second)
      (make-date year month day))))

;; adds days to a date
(define (date-add-days date days)
  (with-normalized-datetime
    (date-year date)
    (date-month date)
    (+ (date-day date) days)
    0
    0
    0
    (lambda (year month day hour minute second)
      (make-date year month day))))

;; adds years to a datetime
(define (datetime-add-years datetime years)
  (make-datetime
    (+ (datetime-year datetime) years)
    (datetime-month datetime)
    (datetime-day datetime)
    (datetime-hour datetime)
    (datetime-minute datetime)
    (datetime-second datetime)))

;; adds months to a datetime
(define (datetime-add-months datetime months)
  (with-normalized-datetime
    (datetime-year datetime)
    (+ (datetime-month datetime) months)
    (datetime-day datetime)
    (datetime-hour datetime)
    (datetime-minute datetime)
    (datetime-second datetime)
    (lambda (year month day hour minute second)
      (make-datetime year month day hour minute second))))

;; adds days to a datetime
(define (datetime-add-days datetime days)
  (with-normalized-datetime
    (datetime-year datetime)
    (datetime-month datetime)
    (+ (datetime-day datetime) days)
    (datetime-hour datetime)
    (datetime-minute datetime)
    (datetime-second datetime)
    (lambda (year month day hour minute second)
      (make-datetime year month day hour minute second))))

;; adds hours to a datetime
(define (datetime-add-hours datetime hours)
  (with-normalized-datetime
    (datetime-year datetime)
    (datetime-month datetime)
    (datetime-day datetime)
    (+ (datetime-hour datetime) hours)
    (datetime-minute datetime)
    (datetime-second datetime)
    (lambda (year month day hour minute second)
      (make-datetime year month day hour minute second))))

;; adds minutes to a datetime
(define (datetime-add-minutes datetime minutes)
  (with-normalized-datetime
    (datetime-year datetime)
    (datetime-month datetime)
    (datetime-day datetime)
    (datetime-hour datetime)
    (+ (datetime-minute datetime) minutes)
    (datetime-second datetime)
    (lambda (year month day hour minute second)
      (make-datetime year month day hour minute second))))

;; adds seconds to a datetime
(define (datetime-add-seconds datetime seconds)
  (with-normalized-datetime
    (datetime-year datetime)
    (datetime-month datetime)
    (datetime-day datetime)
    (datetime-hour datetime)
    (datetime-minute datetime)
    (+ (datetime-second datetime) seconds)
    (lambda (year month day hour minute second)
      (make-datetime year month day hour minute second))))

;; adds hours to a time
(define (time-add-hours time hours)
  (with-normalized-datetime
    0
    0
    0
    (+ (time-hour time) hours)
    (time-minute time)
    (time-second time)
    (lambda (year month day hour minute second)
      (make-time hour minute second))))

;; adds minutes to a time
(define (time-add-minutes time minutes)
  (with-normalized-datetime
    0
    0
    0
    (time-hour time)
    (+ (time-minute time) minutes)
    (time-second time)
    (lambda (year month day hour minute second)
      (make-time hour minute second))))

;; adds seconds to a time
(define (time-add-seconds time seconds)
  (with-normalized-datetime
    0
    0
    0
    (time-hour time)
    (time-minute time)
    (+ (time-second time) seconds)
    (lambda (year month day hour minute second)
      (make-time hour minute second))))
