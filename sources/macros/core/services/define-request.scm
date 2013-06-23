
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-syntax define-request
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; whether a symbols contains a string
      (define (symbol-contains? symbol string)
        (string-contains (symbol->string symbol) string))

      ;; whether a field is a value field
      (define (value-field? field)
        (let ((field-type (cadr field)))
          (and (not (eq? field-type 'list))
               (not (symbol-contains? field-type "subrequest")))))

      ;; whether a field is a value list field
      (define (value-list-field? field)
        (let ((field-type (cadr field)))
          (if (eq? field-type 'list)
            (let* ((element-field (list-ref field 5))
                   (element-field-type (cadr element-field)))
              (not (symbol-contains? element-field-type "subrequest")))
            #f)))

      ;; whether a field is a subrequest field
      (define (subrequest-field? field)
        (let ((field-type (cadr field)))
          (symbol-contains? field-type "subrequest")))

      ;; whether a field is a subrequest list field
      (define (subrequest-list-field? field)
        (let ((field-type (cadr field)))
          (if (eq? field-type 'list)
            (let* ((element-field (list-ref field 5))
                   (element-field-type (cadr element-field)))
              (symbol-contains? element-field-type "subrequest"))
            #f)))

      ;; validates a value field
      (define (validate-value-field field)
        (let ((field-symbol (list-ref field 0))
              (field-type (list-ref field 1))
              (field-validation-parameters (drop field 2)))
          `(validate-value
            field-prefix
            ',field-symbol
            ,field-symbol
            ,(symbol-append 'validate- field-type)
            ',field-validation-parameters)))

      ;; validates a value list field
      (define (validate-value-list-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-required (list-ref field 2))
               (field-min-length (list-ref field 3))
               (field-max-length (list-ref field 4))
               (element-field (list-ref field 5))
               (element-field-symbol (list-ref element-field 0))
               (element-field-type (list-ref element-field 1))
               (element-field-validation-parameters (drop element-field 2)))
          `(validate-value-list
            field-prefix
            ',field-symbol
            ,field-symbol
            ,field-required
            ,field-min-length
            ,field-max-length
            ',element-field-symbol
            ,(symbol-append 'validate- element-field-type)
            ',element-field-validation-parameters)))

      ;; validates a subrequest field
      (define (validate-subrequest-field field)
        (let ((field-symbol (list-ref field 0))
              (field-subrequest-type (list-ref field 1))
              (field-required (list-ref field 2)))
          `(validate-subrequest
            field-prefix
            ',field-symbol
            ,field-symbol
            ,field-required
            ,(symbol-append field-subrequest-type '?)
            ,(symbol-append 'validate- field-subrequest-type))))

      ;; validates a subrequest list field
      (define (validate-subrequest-list-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-required (list-ref field 2))
               (field-min-length (list-ref field 3))
               (field-max-length (list-ref field 4))
               (element-field (list-ref field 5))
               (element-field-symbol (list-ref element-field 0))
               (element-field-subrequest-type (list-ref element-field 1))
               (element-field-required (list-ref element-field 2)))
          `(validate-subrequest-list
            field-prefix
            ',field-symbol
            ,field-symbol
            ,field-required
            ,field-min-length
            ,field-max-length
            ',element-field-symbol
            ,element-field-required
            ,(symbol-append element-field-subrequest-type '?)
            ,(symbol-append 'validate- element-field-subrequest-type))))

      ;; json parses a value field
      (define (json-parse-value-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-symbol-string (symbol->string field-symbol))
               (field-type (list-ref field 1)))
          `(json-upgrade-value
            (json-parse-value
              json-object
              ,field-symbol-string)
            ',field-type)))

      ;; json parses a value list field
      (define (json-parse-value-list-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-symbol-string (symbol->string field-symbol)))
          `(json-parse-value-list
            json-object
            ,field-symbol-string)))

      ;; json parses a subrequest field
      (define (json-parse-subrequest-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-symbol-string (symbol->string field-symbol))
               (field-subrequest-type (list-ref field 1)))
          `(json-parse-subrequest
            json-object
            ,field-symbol-string
            ,(symbol-append 'json-parse- field-subrequest-type))))

      ;; json parses a subrequest list field
      (define (json-parse-subrequest-list-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-symbol-string (symbol->string field-symbol))
               (element-field (list-ref field 5))
               (element-field-subrequest-type (list-ref element-field 1)))
          `(json-parse-subrequest-list
            json-object
            ,field-symbol-string
            ,(symbol-append 'json-parse- element-field-subrequest-type))))

      ;; parses the expression
      (let* ((request-symbol (cadr exp))
             (fields (cddr exp))
             (fields-symbol (map car fields)))
        `(begin

          (use srfi-1)

          (declare (uses json-convert))
          (declare (uses json-parse))
          (declare (uses validation-service-request))

          ;; encapsulates a request
          (define-record ,request-symbol ,@fields-symbol)

          ;; validates a request
          (define (,(symbol-append 'validate- request-symbol) ,request-symbol . args)
            (let (
              (field-prefix (if (not (null? args)) (car args) (string->symbol "")))
              ,@(map
                (lambda (field-symbol)
                  `(,field-symbol
                    (,(symbol-append request-symbol '- field-symbol)
                      ,request-symbol)))
                fields-symbol))
              (append
                ,@(map
                  (lambda (field)
                    (cond ((value-field? field)
                           (validate-value-field field))
                          ((value-list-field? field)
                           (validate-value-list-field field))
                          ((subrequest-field? field)
                           (validate-subrequest-field field))
                          ((subrequest-list-field? field)
                           (validate-subrequest-list-field field))))
                  fields))))

          ;; json parses a request
          (define (,(symbol-append 'json-parse- request-symbol) json-object)
            (let (
              ,@(map
                (lambda (field)
                  (let ((field-symbol (car field)))
                    `(,field-symbol
                      ,(cond ((value-field? field)
                              (json-parse-value-field field))
                             ((value-list-field? field)
                              (json-parse-value-list-field field))
                             ((subrequest-field? field)
                              (json-parse-subrequest-field field))
                             ((subrequest-list-field? field)
                              (json-parse-subrequest-list-field field))))))
                fields))
              (,(symbol-append 'make- request-symbol) ,@fields-symbol))))))))
