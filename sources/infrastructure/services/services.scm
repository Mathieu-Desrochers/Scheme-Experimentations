
(declare (unit services))

(declare (uses sql))

;; invokes a service
(define (invoke-service service request success-procedure error-procedure)

  ;; open a database connection
  (with-sql-connection "/databases/customers.db"
    (lambda (sql-connection)

      ;; disable the synchronous disk
      ;; writes for improved performance
      (sql-disable-synchronous-writes sql-connection)

      ;; start a transaction
      (within-sql-transaction sql-connection
        (lambda ()
          (handle-exceptions exception

            ;; report validation errors
            ;; through the error procedure
            (if (validation-exception? exception)
              (let ((validation-errors (validation-errors exception)))
                (error-procedure validation-errors))
              (abort exception))

            ;; invoke the service
            (let ((response (service sql-connection request)))

              ;; report the response through
              ;; the success procedure
              (success-procedure response))))))))
