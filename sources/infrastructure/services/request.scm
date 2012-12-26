
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-syntax define-request
  (er-macro-transformer
    (lambda (exp rename compare)

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
               (field-required (list-ref field 2))
               (field-min-length (list-ref field 3))
               (field-max-length (list-ref field 4))
               (element-field (list-ref field 5))
               (element-field-symbol (list-ref element-field 0))
               (element-field-type (list-ref element-field 1))
               (element-field-validation-parameters (drop element-field 2)))
        `(,(symbol-append 'validate-request- element-field-type '-list)
          ,field-symbol
          ,field-required
          ,field-min-length
          ,field-max-length
          ',(symbol-append 'invalid- field-symbol)
          ',(symbol-append 'invalid- field-symbol '-length)
          ,@element-field-validation-parameters
          ',(symbol-append 'invalid- element-field-symbol))))

      ;; validates a subrequest field
      (define (validate-subrequest-field field)
        (let ((field-symbol (list-ref field 0))
              (field-required (list-ref field 2))
              (field-subrequest-type (list-ref field 3)))
          `(validate-subrequest
            ,field-symbol
            ,field-required
            ,(symbol-append field-subrequest-type '?)
            ,(symbol-append 'validate- field-subrequest-type)
            ',(symbol-append 'invalid- field-symbol))))

      ;; validates a subrequest list field
      (define (validate-subrequest-list-field field)
        (let* ((field-symbol (list-ref field 0))
               (field-required (list-ref field 2))
               (field-min-length (list-ref field 3))
               (field-max-length (list-ref field 4))
               (element-field (list-ref field 5))
               (element-field-symbol (list-ref element-field 0))
               (element-field-required (list-ref element-field 2))
               (element-field-subrequest-type (list-ref element-field 3)))
          `(validate-subrequest-list
            ,field-symbol
            ,field-required
            ,field-min-length
            ,field-max-length
            ',(symbol-append 'invalid- field-symbol)
            ',(symbol-append 'invalid- field-symbol '-length)
            ,element-field-required
            ,(symbol-append element-field-subrequest-type '?)
            ,(symbol-append 'validate- element-field-subrequest-type)
            ',(symbol-append 'invalid- element-field-symbol))))

      ;; parses the expression
      (let* ((request-symbol (cadr exp))
             (fields (cddr exp))
             (fields-symbol (map car fields)))
        `(begin

          (use srfi-1)

          (declare (uses request-validation))

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
                    (let ((field-type (cadr field)))
                      (if (eq? field-type 'list)
                        (let* ((element-field (list-ref field 5))
                               (element-field-type (cadr element-field)))
                          (if (eq? element-field-type 'subrequest)
                            (validate-subrequest-list-field field)
                            (validate-list-field field)))
                        (if (eq? field-type 'subrequest)
                          (validate-subrequest-field field)
                          (validate-field field)))))
                  fields)))))))))
