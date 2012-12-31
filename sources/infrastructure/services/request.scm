
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-syntax define-request
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; returns the type of a field
      (define (field-type field)
        (let ((field-type (cadr field)))
          (if (eq? field-type 'list)
            (let* ((element-field (list-ref field 5))
                   (element-field-type (cadr element-field)))
              (if (eq? element-field-type 'subrequest)
                'subrequest-list-field
                'list-field))
            (if (eq? field-type 'subrequest)
              'subrequest-field
              'field))))

      ;; validates a field
      (define (validate-field field)
        (let ((field-symbol (list-ref field 0))
              (field-type (list-ref field 1))
              (field-validation-parameters (drop field 2)))
          `(,(symbol-append 'validate-request- field-type)
            ,field-symbol
            ,@field-validation-parameters
            ',(symbol-append 'invalid- field-symbol))))

      ;; validates a list field
      (define (validate-list-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-validation-parameters (take (drop field 2) 3))
               (element-field (list-ref field 5))
               (element-field-symbol (list-ref element-field 0))
               (element-field-type (list-ref element-field 1))
               (element-field-validation-parameters (drop element-field 2)))
        `(,(symbol-append 'validate-request- element-field-type '-list)
          ,field-symbol
          ,@field-validation-parameters
          ',(symbol-append 'invalid- field-symbol)
          ',(symbol-append 'invalid- field-symbol '-length)
          ,@element-field-validation-parameters
          ',(symbol-append 'invalid- element-field-symbol))))

      ;; validates a subrequest field
      (define (validate-subrequest-field field)
        (let ((field-symbol (list-ref field 0))
              (field-validation-parameters (take (drop field 2) 1))
              (field-subrequest-type (list-ref field 3)))
          `(validate-subrequest
            ,field-symbol
            ,@field-validation-parameters
            ,(symbol-append field-subrequest-type '?)
            ,(symbol-append 'validate- field-subrequest-type)
            ',(symbol-append 'invalid- field-symbol))))

      ;; validates a subrequest list field
      (define (validate-subrequest-list-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-validation-parameters (take (drop field 2) 3))
               (element-field (list-ref field 5))
               (element-field-symbol (list-ref element-field 0))
               (element-field-validation-parameters (take (drop element-field 2) 1))
               (element-field-subrequest-type (list-ref element-field 3)))
          `(validate-subrequest-list
            ,field-symbol
            ,@field-validation-parameters
            ',(symbol-append 'invalid- field-symbol)
            ',(symbol-append 'invalid- field-symbol '-length)
            ,@element-field-validation-parameters
            ,(symbol-append element-field-subrequest-type '?)
            ,(symbol-append 'validate- element-field-subrequest-type)
            ',(symbol-append 'invalid- element-field-symbol))))

      ;; parses a field
      (define (parse-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-symbol-string (symbol->string field-symbol)))
          `(let ((json-object-property (json-object-property json-object ,field-symbol-string)))
            (if json-object-property
              (json-object-value json-object-property)
              #f))))

      ;; parses a list field
      (define (parse-list-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-symbol-string (symbol->string field-symbol)))
          `(let ((json-object-property (json-object-property json-object ,field-symbol-string)))
            (if json-object-property
              (let ((json-object-array-elements (json-object-array-elements json-object-property)))
                (if json-object-array-elements
                  (map
                    json-object-value
                    json-object-array-elements)
                  #f))
              #f))))

      ;; parses a subrequest field
      (define (parse-subrequest-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-symbol-string (symbol->string field-symbol))
               (field-subrequest-type (list-ref field 3)))
          `(let ((json-object-property (json-object-property json-object ,field-symbol-string)))
            (if json-object-property
              (,(symbol-append 'parse- field-subrequest-type) json-object-property)
              #f))))

      ;; parses the expression
      (let* ((request-symbol (cadr exp))
             (fields (cddr exp))
             (fields-symbol (map car fields)))
        `(begin

          (use srfi-1)

          (declare (uses request-validation))
          (declare (uses json))

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
                    (let ((field-type (field-type field)))
                      (cond ((eq? field-type 'field) (validate-field field))
                            ((eq? field-type 'list-field) (validate-list-field field))
                            ((eq? field-type 'subrequest-field) (validate-subrequest-field field))
                            ((eq? field-type 'subrequest-list-field) (validate-subrequest-list-field field)))))
                  fields))))

          ;; parses a request
          (define (,(symbol-append 'parse- request-symbol) json-object)
            (let (
              ,@(map
                (lambda (field)
                  (let ((field-symbol (car field))
                        (field-type (field-type field)))
                    `(,field-symbol
                      ,(cond ((eq? field-type 'field) (parse-field field))
                             ((eq? field-type 'list-field) (parse-list-field field))
                             ((eq? field-type 'subrequest-field) (parse-subrequest-field field))))))
                fields))
              (,(symbol-append 'make- request-symbol) ,@fields-symbol))))))))
