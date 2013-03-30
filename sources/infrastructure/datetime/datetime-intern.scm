
(declare (unit datetime-intern))

(declare (uses scdtl))

;; invokes a procedure with a scdtl-tm*
(define (datetime-with-scdtl-tm* procedure)
  (let ((scdtl-tm* (malloc-scdtl-tm)))
    (when (not scdtl-tm*)
      (abort "could not allocate scdtl tm"))
    (handle-exceptions exception
      (begin
        (free-scdtl-tm scdtl-tm*)
        (abort exception))
      (let ((procedure-result (procedure scdtl-tm*)))
        (free-scdtl-tm scdtl-tm*)
        procedure-result))))

;; invokes a procedure with a parsed scdtl-tm*
(define (datetime-with-parsed-scdtl-tm* string format procedure)
  (datetime-with-scdtl-tm*
    (lambda (scdtl-tm*)
      (let ((scdtl-strptime-result (scdtl-strptime string format scdtl-tm*)))
        (if (eq? scdtl-strptime-result 0)
          #f
          (procedure scdtl-tm*))))))

;; formats a datetime
(define (format-datetime year month day hour minute second format)
  (datetime-with-scdtl-tm*
    (lambda (scdtl-tm*)
      (scdtl-assign-tm scdtl-tm* (- year 1900) (- month 1) day hour minute second)
      (let ((scdtl-strftime-result* (scdtl-strftime-wrapped scdtl-tm* format)))
        (when (not scdtl-strftime-result*)
          (abort "could not format datetime"))
        (let ((scdtl-strftime-result-value (scdtl-strftime-result-value scdtl-strftime-result*)))
          (free-scdtl-strftime-result scdtl-strftime-result*)
          scdtl-strftime-result-value)))))

;; invokes a procedure with normalized datetime values
(define (with-normalized-datetime year month day hour minute second procedure)
  (datetime-with-scdtl-tm*
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

;; invokes a procedure with the current datetime
(define (with-datetime-now procedure)
  (let* ((time-since-epoch (scdtl-time))
         (scdtl-tm* (scdtl-gmtime time-since-epoch)))
    (procedure
      (+ (scdtl-tm-year scdtl-tm*) 1900)
      (+ (scdtl-tm-mon scdtl-tm*) 1)
      (scdtl-tm-mday scdtl-tm*)
      (scdtl-tm-hour scdtl-tm*)
      (scdtl-tm-min scdtl-tm*)
      (scdtl-tm-sec scdtl-tm*))))
