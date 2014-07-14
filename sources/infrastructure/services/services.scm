
(declare (unit services))

(declare (uses sql))

;; invokes a service
(define (invoke-service service request success-procedure validation-errors-procedure)

  ;; open a database connection
  (with-sql-connection "/var/databases/customers.db"
    (lambda (sql-connection)

      ;; enable the foreign keys enforcement
      (sql-enable-foreign-keys sql-connection)

      ;; disable the synchronous disk
      ;; writes for improved performance
      (sql-disable-synchronous-writes sql-connection)

      ;; start a transaction
      (sql-begin-transaction sql-connection)

      ;; check for exceptions
      (handle-exceptions
        exception
        (begin

          ;; rollback the transaction
          (with-exception-hiding
            (lambda ()
              (sql-rollback-transaction sql-connection)))

          ;; check if a validation exception was raised
          (if (validation-exception? exception)

            ;; report the validation errors
            ;; through the error procedure
            (validation-errors-procedure (validation-errors exception))

            ;; re-raise any other exception
            (abort exception)))

        ;; invoke the service
        (let ((response (service sql-connection request)))

          ;; commit the transaction
          (sql-commit-transaction sql-connection)

          ;; report the response through
          ;; the success procedure
          (success-procedure response))))))
