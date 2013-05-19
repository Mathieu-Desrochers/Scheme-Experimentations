
(declare (unit date-time-intern))

(declare (uses exceptions))
(declare (uses scdtl))

;; invokes a procedure with a scdtl-tm*
(define (date-time-with-scdtl-tm* procedure)
  (define (checked-malloc-scdtl-tm)
    (let ((scdtl-tm* (malloc-scdtl-tm)))
      (when (not scdtl-tm*)
        (abort "failed to allocate scdtl tm"))
      scdtl-tm*))
  (with-guaranteed-release
    checked-malloc-scdtl-tm
    procedure
    free-scdtl-tm))

;; invokes a procedure with a parsed scdtl-tm*
(define (date-time-with-parsed-scdtl-tm* string format procedure)
  (date-time-with-scdtl-tm*
    (lambda (scdtl-tm*)
      (let ((scdtl-strptime-result (scdtl-strptime string format scdtl-tm*)))
        (if (eq? scdtl-strptime-result 0)
          #f
          (procedure scdtl-tm*))))))

;; formats a date-time
(define (format-date-time year month day hour minute second format)
  (date-time-with-scdtl-tm*
    (lambda (scdtl-tm*)
      (scdtl-assign-tm scdtl-tm* (- year 1900) (- month 1) day hour minute second)
      (let ((scdtl-strftime-result* (scdtl-strftime-wrapped scdtl-tm* format)))
        (when (not scdtl-strftime-result*)
          (abort "failed to format date-time"))
        (let ((scdtl-strftime-result-value (scdtl-strftime-result-value scdtl-strftime-result*)))
          (free-scdtl-strftime-result scdtl-strftime-result*)
          scdtl-strftime-result-value)))))

;; invokes a procedure with normalized date-time values
(define (with-normalized-date-time year month day hour minute second procedure)
  (date-time-with-scdtl-tm*
    (lambda (scdtl-tm*)
      (scdtl-assign-tm scdtl-tm* (- year 1900) (- month 1) day hour minute second)
      (let* ((time-since-epoch (scdtl-timegm scdtl-tm*))
             (normalized-scdtl-tm* (scdtl-gmtime time-since-epoch)))
          (procedure
            (+ (scdtl-tm-year normalized-scdtl-tm*) 1900)
            (+ (scdtl-tm-mon normalized-scdtl-tm*) 1)
            (scdtl-tm-mday normalized-scdtl-tm*)
            (scdtl-tm-hour normalized-scdtl-tm*)
            (scdtl-tm-min normalized-scdtl-tm*)
            (scdtl-tm-sec normalized-scdtl-tm*))))))

;; invokes a procedure with the current date-time
(define (with-date-time-now procedure)
  (let* ((time-since-epoch (scdtl-time))
         (scdtl-tm* (scdtl-gmtime time-since-epoch)))
    (procedure
      (+ (scdtl-tm-year scdtl-tm*) 1900)
      (+ (scdtl-tm-mon scdtl-tm*) 1)
      (scdtl-tm-mday scdtl-tm*)
      (scdtl-tm-hour scdtl-tm*)
      (scdtl-tm-min scdtl-tm*)
      (scdtl-tm-sec scdtl-tm*))))
