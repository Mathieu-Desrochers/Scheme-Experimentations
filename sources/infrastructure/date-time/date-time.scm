
(declare (unit date-time))

(declare (uses date-time-intern))
(declare (uses scdtl))

;; encapsulates a date
(define-record date year month day)

;; encapsulates a date-time expressed in UTC
(define-record date-time year month day hour minute second)

;; encapsulates a date-time-without-seconds expressed in UTC
(define-record date-time-without-seconds year month day hour minute)

;; encapsulates a time
(define-record time hour minute second)

;; encapsulates a day-of-week
(define-record day-of-week name)

;; parses a string representing a date
(define (string->date string)
  (date-time-with-parsed-scdtl-tm* string "%Y-%m-%d"
    (lambda (scdtl-tm*)
      (make-date
        (+ (scdtl-tm-year scdtl-tm*) 1900)
        (+ (scdtl-tm-mon scdtl-tm*) 1)
        (scdtl-tm-mday scdtl-tm*)))))

;; parses a string representing a date-time
(define (string->date-time string)
  (date-time-with-parsed-scdtl-tm* string "%Y-%m-%dT%H:%M:%SZ"
    (lambda (scdtl-tm*)
      (make-date-time
        (+ (scdtl-tm-year scdtl-tm*) 1900)
        (+ (scdtl-tm-mon scdtl-tm*) 1)
        (scdtl-tm-mday scdtl-tm*)
        (scdtl-tm-hour scdtl-tm*)
        (scdtl-tm-min scdtl-tm*)
        (scdtl-tm-sec scdtl-tm*)))))

;; parses a string representing a date-time-without-seconds
(define (string->date-time-without-seconds string)
  (date-time-with-parsed-scdtl-tm* string "%Y-%m-%dT%H:%MZ"
    (lambda (scdtl-tm*)
      (make-date-time-without-seconds
        (+ (scdtl-tm-year scdtl-tm*) 1900)
        (+ (scdtl-tm-mon scdtl-tm*) 1)
        (scdtl-tm-mday scdtl-tm*)
        (scdtl-tm-hour scdtl-tm*)
        (scdtl-tm-min scdtl-tm*)))))

;; parses a string representing a time
(define (string->time string)
  (date-time-with-parsed-scdtl-tm* string "%H:%M:%S"
    (lambda (scdtl-tm*)
      (make-time
        (scdtl-tm-hour scdtl-tm*)
        (scdtl-tm-min scdtl-tm*)
        (scdtl-tm-sec scdtl-tm*)))))

;; parses a string representing a day-of-week
(define (string->day-of-week string)
  (make-day-of-week string))

;; parses an integer representing a day-of-week
(define (integer->day-of-week integer)
  (cond ((equal? integer 0) (make-day-of-week "Sunday"))
        ((equal? integer 1) (make-day-of-week "Monday"))
        ((equal? integer 2) (make-day-of-week "Tuesday"))
        ((equal? integer 3) (make-day-of-week "Wednesday"))
        ((equal? integer 4) (make-day-of-week "Thursday"))
        ((equal? integer 5) (make-day-of-week "Friday"))
        ((equal? integer 6) (make-day-of-week "Saturday"))))

;; serializes a date to string
(define (date->string date)
  (format-date-time
    (date-year date)
    (date-month date)
    (date-day date)
    0
    0
    0
    "%Y-%m-%d"))

;; serializes a date-time to string
(define (date-time->string date-time)
  (format-date-time
    (date-time-year date-time)
    (date-time-month date-time)
    (date-time-day date-time)
    (date-time-hour date-time)
    (date-time-minute date-time)
    (date-time-second date-time)
    "%Y-%m-%dT%H:%M:%SZ"))

;; serializes a date-time-without-seconds to string
(define (date-time-without-seconds->string date-time-without-seconds)
  (format-date-time
    (date-time-without-seconds-year date-time-without-seconds)
    (date-time-without-seconds-month date-time-without-seconds)
    (date-time-without-seconds-day date-time-without-seconds)
    (date-time-without-seconds-hour date-time-without-seconds)
    (date-time-without-seconds-minute date-time-without-seconds)
    0
    "%Y-%m-%dT%H:%MZ"))

;; serializes a time to string
(define (time->string time)
  (format-date-time
    0
    0
    0
    (time-hour time)
    (time-minute time)
    (time-second time)
    "%H:%M:%S"))

;; serializes a day-of-week to string
(define (day-of-week->string day-of-week)
  (day-of-week-name day-of-week))

;; serializes a day-of-week to integer
(define (day-of-week->integer day-of-week)
  (let ((day-of-week-name (day-of-week-name day-of-week)))
    (cond ((equal? day-of-week-name "Sunday") 0)
          ((equal? day-of-week-name "Monday") 1)
          ((equal? day-of-week-name "Tuesday") 2)
          ((equal? day-of-week-name "Wednesday") 3)
          ((equal? day-of-week-name "Thursday") 4)
          ((equal? day-of-week-name "Friday") 5)
          ((equal? day-of-week-name "Saturday") 6))))

;; returns whether a date is valid
(define (date-valid? date)
  (with-normalized-date-time
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

;; returns whether a date-time is valid
(define (date-time-valid? date-time)
  (with-normalized-date-time
    (date-time-year date-time)
    (date-time-month date-time)
    (date-time-day date-time)
    (date-time-hour date-time)
    (date-time-minute date-time)
    (date-time-second date-time)
    (lambda (year month day hour minute second)
      (let ((normalized-date-time (make-date-time year month day hour minute second)))
        (and (equal? date-time normalized-date-time)
             (> year 1000))))))

;; returns whether a date-time-without-seconds is valid
(define (date-time-without-seconds-valid? date-time-without-seconds)
  (with-normalized-date-time
    (date-time-without-seconds-year date-time-without-seconds)
    (date-time-without-seconds-month date-time-without-seconds)
    (date-time-without-seconds-day date-time-without-seconds)
    (date-time-without-seconds-hour date-time-without-seconds)
    (date-time-without-seconds-minute date-time-without-seconds)
    0
    (lambda (year month day hour minute second)
      (let ((normalized-date-time-without-seconds (make-date-time-without-seconds year month day hour minute)))
        (and (equal? date-time-without-seconds normalized-date-time-without-seconds)
             (> year 1000))))))

;; returns whether a time is valid
(define (time-valid? time)
  (with-normalized-date-time
    0
    0
    0
    (time-hour time)
    (time-minute time)
    (time-second time)
    (lambda (year month day hour minute second)
      (let ((normalized-time (make-time hour minute second)))
        (equal? time normalized-time)))))

;; returns whether a day-of-week is valid
(define (day-of-week-valid? day-of-week)
  (let ((day-of-week-name (day-of-week-name day-of-week)))
    (or (equal? day-of-week-name "Sunday")
        (equal? day-of-week-name "Monday")
        (equal? day-of-week-name "Tuesday")
        (equal? day-of-week-name "Wednesday")
        (equal? day-of-week-name "Thursday")
        (equal? day-of-week-name "Friday")
        (equal? day-of-week-name "Saturday"))))

;; returns the current date
(define (date-now)
  (with-date-time-now
    (lambda (year month day hour minute second)
      (make-date year month day))))

;; returns the current date-time
(define (date-time-now)
  (with-date-time-now
    (lambda (year month day hour minute second)
      (make-date-time year month day hour minute second))))

;; returns the current time
(define (time-now)
  (with-date-time-now
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
  (with-normalized-date-time
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
  (with-normalized-date-time
    (date-year date)
    (date-month date)
    (+ (date-day date) days)
    0
    0
    0
    (lambda (year month day hour minute second)
      (make-date year month day))))

;; adds years to a date-time
(define (date-time-add-years date-time years)
  (make-date-time
    (+ (date-time-year date-time) years)
    (date-time-month date-time)
    (date-time-day date-time)
    (date-time-hour date-time)
    (date-time-minute date-time)
    (date-time-second date-time)))

;; adds months to a date-time
(define (date-time-add-months date-time months)
  (with-normalized-date-time
    (date-time-year date-time)
    (+ (date-time-month date-time) months)
    (date-time-day date-time)
    (date-time-hour date-time)
    (date-time-minute date-time)
    (date-time-second date-time)
    (lambda (year month day hour minute second)
      (make-date-time year month day hour minute second))))

;; adds days to a date-time
(define (date-time-add-days date-time days)
  (with-normalized-date-time
    (date-time-year date-time)
    (date-time-month date-time)
    (+ (date-time-day date-time) days)
    (date-time-hour date-time)
    (date-time-minute date-time)
    (date-time-second date-time)
    (lambda (year month day hour minute second)
      (make-date-time year month day hour minute second))))

;; adds hours to a date-time
(define (date-time-add-hours date-time hours)
  (with-normalized-date-time
    (date-time-year date-time)
    (date-time-month date-time)
    (date-time-day date-time)
    (+ (date-time-hour date-time) hours)
    (date-time-minute date-time)
    (date-time-second date-time)
    (lambda (year month day hour minute second)
      (make-date-time year month day hour minute second))))

;; adds minutes to a date-time
(define (date-time-add-minutes date-time minutes)
  (with-normalized-date-time
    (date-time-year date-time)
    (date-time-month date-time)
    (date-time-day date-time)
    (date-time-hour date-time)
    (+ (date-time-minute date-time) minutes)
    (date-time-second date-time)
    (lambda (year month day hour minute second)
      (make-date-time year month day hour minute second))))

;; adds seconds to a date-time
(define (date-time-add-seconds date-time seconds)
  (with-normalized-date-time
    (date-time-year date-time)
    (date-time-month date-time)
    (date-time-day date-time)
    (date-time-hour date-time)
    (date-time-minute date-time)
    (+ (date-time-second date-time) seconds)
    (lambda (year month day hour minute second)
      (make-date-time year month day hour minute second))))

;; adds hours to a time
(define (time-add-hours time hours)
  (with-normalized-date-time
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
  (with-normalized-date-time
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
  (with-normalized-date-time
    0
    0
    0
    (time-hour time)
    (time-minute time)
    (+ (time-second time) seconds)
    (lambda (year month day hour minute second)
      (make-time hour minute second))))
