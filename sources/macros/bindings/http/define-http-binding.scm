
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-syntax define-http-binding
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((method (list-ref exp 1))
             (route (list-ref exp 2))
             (service-symbol (list-ref exp 3))
             (parse-request-procedure-symbol (list-ref exp 4))
             (format-response-procedure-symbol (list-ref exp 5)))
        `(begin

          (declare (uses http))

          ;; returns the http registration
          (define (,(symbol-append service-symbol '-http-registration))
            (make-http-registration
              ,method
              ,route
              ,service-symbol
              ,parse-request-procedure-symbol
              ,format-response-procedure-symbol)))))))
