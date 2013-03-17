
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

;; serializes a scdtl-tm to string
(define (datetime-serialize-scdtl-tm year month day hour minute second format)
  (datetime-with-scdtl-tm*
    (lambda (scdtl-tm*)
      (scdtl-assign-tm scdtl-tm* (- year 1900) (- month 1) day hour minute second)
      (let ((scdtl-strftime-result* (scdtl-strftime-wrapped scdtl-tm* format)))
        (when (not scdtl-strftime-result*)
          (abort "could not serialize scdtl tm"))
        (let ((scdtl-strftime-result-value (scdtl-strftime-result-value scdtl-strftime-result*)))
          (free-scdtl-strftime-result scdtl-strftime-result*)
          scdtl-strftime-result-value)))))
