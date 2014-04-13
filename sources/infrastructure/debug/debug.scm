
(declare (unit debug))

(declare (uses ports))

;; prints a message to the error port
(define (debug-print message)
  (with-output-to-port
    (current-error-port)
    (lambda ()
      (print message))))
