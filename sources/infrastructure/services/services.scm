
(declare (unit services))

(declare (uses sql))

;; invokes a service
(define (invoke-service service request success-procedure error-procedure)
  (with-sql-connection "/databases/customers.db"
    (lambda (sql-connection)
      (handle-exceptions exception
        (if (validation-exception? exception)
          (let ((validation-errors (validation-errors exception)))
            (error-procedure validation-errors))
          (abort exception))
      (let ((response (service sql-connection request)))
        (success-procedure response))))))
