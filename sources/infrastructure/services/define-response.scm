
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-syntax define-response
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; whether a field is a value field
      (define (value-field? field)
        (eq? (length field) 1))

      ;; whether a field is a value list field
      (define (value-list-field? field)
        (and (eq? (length field) 2)
             (eq? (cadr field) 'list)))

      ;; formats a value field
      (define (format-value-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-symbol-string (symbol->string field-symbol)))
          `(json-format-value
            json-object
            ,field-symbol-string
            ,field-symbol)))

      ;; formats a value list field
      (define (format-value-list-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-symbol-string (symbol->string field-symbol)))
          `(json-format-value-list
            json-object
            ,field-symbol-string
            ,field-symbol)))

      ;; parses the expression
      (let* ((response-symbol (cadr exp))
             (fields (cddr exp))
             (fields-symbol (map car fields)))
        `(begin

          (use srfi-1)

          (declare (uses json-format))

          ;; encapsulates a response
          (define-record ,response-symbol ,@fields-symbol)

          ;; formats a response
          (define (,(symbol-append 'format- response-symbol) ,response-symbol json-object)
            (let (
              ,@(map
                (lambda (field-symbol)
                  `(,field-symbol (,(symbol-append response-symbol '- field-symbol) ,response-symbol)))
                fields-symbol))
              ,@(map
                (lambda (field)
                  (cond ((value-field? field) (format-value-field field))
                        ((value-list-field? field) (format-value-list-field field))))
                fields))))))))
