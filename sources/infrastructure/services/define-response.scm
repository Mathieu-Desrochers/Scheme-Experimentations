
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-syntax define-response
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((response-symbol (cadr exp))
             (fields (cddr exp))
             (fields-symbol (map car fields)))
        `(begin

          (use srfi-1)

          ;; encapsulates a response
          (define-record ,response-symbol ,@fields-symbol))))))
