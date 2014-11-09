
(use extras)
(use srfi-1)
(use srfi-13)

(declare (unit test))

(declare (uses sql))

;; encapsulates a test step
(define-record test-step service request response)

;; runs a test composed of multiple steps
(define (test-run name test-steps)

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

      ;; provide feedback
      (printf (string-append name "... "))

      ;; check if all the services
      ;; return their expected response
      (if (every
            (lambda (test-step)

              ;; invoke the service
              (let ((actual-response
                      ((test-step-service test-step)
                        sql-connection
                        (test-step-request test-step))))

                ;; compare the response
                (equal?
                  actual-response
                  (test-step-response test-step))))

            test-steps)

        ;; provide feedback
        (printf "passed\n")
        (printf "failed\n"))

      ;; rollback the transaction
      (sql-rollback-transaction sql-connection))))
