
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
        (let ((field-symbol (list-ref field 0))
              (required (list-ref field 2))
              (min-length (list-ref field 3))
              (max-length (list-ref field 4))
              (element-symbol (list-ref field 5))
              (element-type (list-ref field 6))
              (element-validation-parameters (drop field 7)))
        `(,(symbol-append 'validate-request- element-type '-list)
          ,field-symbol
          ,required
          ,min-length
          ,max-length
          ',(symbol-append 'invalid- field-symbol)
          ',(symbol-append 'invalid- field-symbol '-length)
          ,@element-validation-parameters
          ',(symbol-append 'invalid- element-symbol))))

      ;; validates a subrequest field
      (define (validate-subrequest-field field)
        (let ((field-symbol (list-ref field 0))
              (required (list-ref field 2))
              (request-type (list-ref field 3)))
          `(validate-subrequest
            ,field-symbol
            ,required
            ,(symbol-append request-type '?)
            ,(symbol-append 'validate- request-type)
            ',(symbol-append 'invalid- field-symbol))))

      ;; validates a subrequest list field
      (define (validate-subrequest-list-field field)
        (let ((field-symbol (list-ref field 0))
              (required (list-ref field 3))
              (min-length (list-ref field 4))
              (max-length (list-ref field 5))
              (element-symbol (list-ref field 6))
              (request-type (list-ref field 7)))
          `(validate-subrequest-list
            ,field-symbol
            ,required
            ,min-length
            ,max-length
            ',(symbol-append 'invalid- field-symbol)
            ',(symbol-append 'invalid- field-symbol '-length)
            ,(symbol-append request-type '?)
            ,(symbol-append 'validate- request-type)
            ',(symbol-append 'invalid- element-symbol))))

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
                    (let ((field-type (cadr field))
                          (field-subtype (caddr field)))
                      (cond ((and (eq? field-type 'list) (eq? field-subtype 'request))
                              (validate-subrequest-list-field field))
                            ((eq? field-type 'list)
                              (validate-list-field field))
                            ((eq? field-type 'request)
                              (validate-subrequest-field field))
                            (else
                              (validate-field field)))))
                  fields)))))))))
