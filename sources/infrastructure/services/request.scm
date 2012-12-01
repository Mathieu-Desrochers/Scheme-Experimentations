
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-syntax define-request
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; validates a field
      (define (validate-field field-symbol field-type-symbol)
        `(if (not (,(symbol-append 'validate- field-type-symbol) ,field-symbol))
          (list (cons ',(symbol-append 'invalid- field-symbol) ,field-symbol))
          '()))

      ;; parses the expression
      (let* ((request-symbol (cadr exp))
             (fields (cddr exp))
             (fields-symbol (map car fields)))
        `(begin

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
                    (let ((field-symbol (car field))
                          (field-type-symbol (cadr field)))
                      (validate-field field-symbol field-type-symbol)))
                  fields)))))))))
