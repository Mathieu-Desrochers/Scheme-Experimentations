
(declare (unit date-time))

(declare (uses date-time-intern))
(declare (uses scdtl))

;; encapsulates a date
(define-record date year month day)

;; encapsulates a date time expressed in UTC
(define-record date-time year month day hour minute second)

;; encapsulates a time
(define-record time hour minute second)

;; encapsulates a day of week
(define-record day-of-week name)

;; parses a string representing a date
(define (string->date string)
  (date-time-with-parsed-scdtl-tm* string "%Y-%m-%d"
    (lambda (scdtl-tm*)
      (make-date
        (+ (scdtl-tm-year scdtl-tm*) 1900)
        (+ (scdtl-tm-mon scdtl-tm*) 1)
        (scdtl-tm-mday scdtl-tm*)))))

;; parses a string representing a UTC date time
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

;; parses a string representing a time
(define (string->time* string)
  (date-time-with-parsed-scdtl-tm* string "%H:%M:%S"
    (lambda (scdtl-tm*)
      (make-time
        (scdtl-tm-hour scdtl-tm*)
        (scdtl-tm-min scdtl-tm*)
        (scdtl-tm-sec scdtl-tm*)))))

;; parses a string representing a day of week
(define (string->day-of-week string)
  (make-day-of-week string))

;; parses an integer representing a day of week
(define (integer->day-of-week integer)
  (cond ((equal? integer 0) (make-day-of-week "Sunday"))
        ((equal? integer 1) (make-day-of-week "Monday"))
        ((equal? integer 2) (make-day-of-week "Tuesday"))
        ((equal? integer 3) (make-day-of-week "Wednesday"))
        ((equal? integer 4) (make-day-of-week "Thursday"))
        ((equal? integer 5) (make-day-of-week "Friday"))
        ((equal? integer 6) (make-day-of-week "Saturday"))
        (else (make-day-of-week ""))))

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

;; serializes a date time to string
(define (date-time->string date-time)
  (format-date-time
    (date-time-year date-time)
    (date-time-month date-time)
    (date-time-day date-time)
    (date-time-hour date-time)
    (date-time-minute date-time)
    (date-time-second date-time)
    "%Y-%m-%dT%H:%M:%SZ"))

;; serializes a time to string
(define (time->string* time)
  (format-date-time
    0
    0
    0
    (time-hour time)
    (time-minute time)
    (time-second time)
    "%H:%M:%S"))

;; serializes a day of week to string
(define (day-of-week->string day-of-week)
  (day-of-week-name day-of-week))

;; serializes a day of week to integer
(define (day-of-week->integer day-of-week)
  (let ((day-of-week-name (day-of-week-name day-of-week)))
    (cond ((equal? day-of-week-name "Sunday") 0)
          ((equal? day-of-week-name "Monday") 1)
          ((equal? day-of-week-name "Tuesday") 2)
          ((equal? day-of-week-name "Wednesday") 3)
          ((equal? day-of-week-name "Thursday") 4)
          ((equal? day-of-week-name "Friday") 5)
          ((equal? day-of-week-name "Saturday") 6)
          (else #f))))

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

;; returns whether a date time is valid
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

;; returns whether a day of week is valid
(define (day-of-week-valid? day-of-week)
  (let ((day-of-week-name (day-of-week-name day-of-week)))
    (or (equal? day-of-week-name "Sunday")
        (equal? day-of-week-name "Monday")
        (equal? day-of-week-name "Tuesday")
        (equal? day-of-week-name "Wednesday")
        (equal? day-of-week-name "Thursday")
        (equal? day-of-week-name "Friday")
        (equal? day-of-week-name "Saturday"))))

;; returns the current UTC date
(define (date-now)
  (with-date-time-now
    (lambda (year month day hour minute second)
      (make-date year month day))))

;; returns the current UTC date time
(define (date-time-now)
  (with-date-time-now
    (lambda (year month day hour minute second)
      (make-date-time year month day hour minute second))))

;; returns the current UTC time
(define (time-now)
  (with-date-time-now
    (lambda (year month day hour minute second)
      (make-time hour minute second))))

;; adds two dates
(define (date-add first-date second-date)
  (with-normalized-date-time
    (+ (date-year first-date) (date-year second-date))
    (+ (date-month first-date) (date-month second-date))
    (+ (date-day first-date) (date-day second-date))
    0
    0
    0
    (lambda (year month day hour minute second)
      (make-date year month day))))

;; adds two date times
(define (date-time-add first-date-time second-date-time)
  (with-normalized-date-time
    (+ (date-time-year first-date-time) (date-time-year second-date-time))
    (+ (date-time-month first-date-time) (date-time-month second-date-time))
    (+ (date-time-day first-date-time) (date-time-day second-date-time))
    (+ (date-time-hour first-date-time) (date-time-hour second-date-time))
    (+ (date-time-minute first-date-time) (date-time-minute second-date-time))
    (+ (date-time-second first-date-time) (date-time-second second-date-time))
    (lambda (year month day hour minute second)
      (make-date-time year month day hour minute second))))

;; adds two times
(define (time-add first-time second-time)
  (with-normalized-date-time
    0
    0
    0
    (+ (time-hour first-time) (time-hour second-time))
    (+ (time-minute first-time) (time-minute second-time))
    (+ (time-second first-time) (time-second second-time))
    (lambda (year month day hour minute second)
      (make-time hour minute second))))

;; calculates an age based on a birthdate
(define (date-calculate-age birthdate)

  ;; calculate the difference
  ;; between the birthdate and today
  (let* ((negative-birthdate
            (make-date
              (* -1 (date-year birthdate))
              (* -1 (date-month birthdate))
              (* -1 (date-day birthdate))))
         (date-difference
            (date-add
              (date-now)
              negative-birthdate)))

    ;; calculate the age
    (+ (date-year date-difference)
       (/ (date-month date-difference) 12)
       (/ (date-day date-difference) 365))))
