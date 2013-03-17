
(declare (unit datetime-intern))

(declare (uses scdtl))

;; invokes a procedure with a scdtl-tm*
(define (datetime-with-scdtl-tm* procedure)
  (let ((scdtl-tm* (scdtl-malloc-tm)))
    (when (not scdtl-tm*)
      (abort "could not allocate scdtl tm"))
    (handle-exceptions exception
      (begin
        (scdtl-free-tm scdtl-tm*)
        (abort exception))
      (let ((procedure-result (procedure scdtl-tm*)))
        (scdtl-free-tm scdtl-tm*)
        procedure-result))))

;; invokes a procedure with a parsed scdtl-tm*
(define (datetime-with-parsed-scdtl-tm* string format procedure)
  (datetime-with-scdtl-tm*
    (lambda (scdtl-tm*)
      (let ((scdtl-strptime-result (scdtl-strptime string format scdtl-tm*)))
        (if (eq? scdtl-strptime-result 0)
          #f
          (procedure scdtl-tm*))))))
