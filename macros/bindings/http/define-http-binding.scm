
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-syntax define-http-binding
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((method (list-ref exp 1))
             (route (list-ref exp 2))
             (content-type (list-ref exp 3))
             (service-symbol (list-ref exp 4))
             (parse-request-procedure-symbol (list-ref exp 5))
             (format-response-procedure-symbol (list-ref exp 6)))
        `(begin

          (declare (uses http))

          ;; returns the http binding
          (define (,(symbol-append 'make- service-symbol '-http-binding))
            (make-http-binding
              ,method
              ,route
              ,content-type
              ,service-symbol
              ,parse-request-procedure-symbol
              ,format-response-procedure-symbol)))))))
