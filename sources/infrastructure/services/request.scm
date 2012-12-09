
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-syntax define-request
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; validates a field
      (define (validate-field field-symbol field-type field-details)
        `(,(symbol-append 'validate-request- field-type) ,field-symbol ,@field-details ',(symbol-append 'invalid- field-symbol)))

      ;; parses the expression
      (let* ((request-symbol (cadr exp))
             (fields (cddr exp))
             (fields-symbol (map car fields)))
        `(begin

          ;(use srfi-1)

          ;(declare (unit ,request-symbol))
          ;(declare (uses request-validation))

          ;; encapsulates a request
          (define-record ,request-symbol ,@fields-symbol)

          ;; validates a request
          (define (,(symbol-append 'validate- request-symbol) ,request-symbol)
            (let (
              ,@(map
                (lambda (field-symbol)
                  `(,field-symbol (,(symbol-append request-symbol '- field-symbol) ,request-symbol)))
                fields-symbol))
              (append
                ,@(map
                  (lambda (field)
                    (let* ((field-symbol (car field))
                           (field-type (cadr field))
                           (field-details (cddr field)))
                      (validate-field field-symbol field-type field-details)))
                  fields)))))))))
