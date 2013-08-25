
(declare (unit json-format))

(declare (uses json))

;; formats a value
(define (json-format-value json-object property-name value value-is-boolean)
  (when (or value value-is-boolean)
    (with-new-json-object-from-value value
      (lambda (json-object-value)
        (json-object-property-set! json-object property-name json-object-value)))))

;; formats a value list
(define (json-format-value-list json-object property-name value)
  (with-new-json-object-array
    (lambda (json-object-array)
      (map
        (lambda (element-value)
          (with-new-json-object-from-value element-value
            (lambda (json-object-element)
              (json-object-array-append! json-object-array json-object-element))))
        value)
      (json-object-property-set! json-object property-name json-object-array))))

;; formats a subresponse
(define (json-format-subresponse json-object property-name value format-subresponse-procedure)
  (when value
    (with-new-json-object
      (lambda (json-object-subresponse)
        (format-subresponse-procedure value json-object-subresponse)
        (json-object-property-set! json-object property-name json-object-subresponse)))))

;; formats a subresponse list
(define (json-format-subresponse-list json-object property-name value format-subresponse-procedure)
  (with-new-json-object-array
    (lambda (json-object-array)
      (map
        (lambda (element-value)
          (with-new-json-object
            (lambda (json-object-element)
              (format-subresponse-procedure element-value json-object-element)
              (json-object-array-append! json-object-array json-object-element))))
        value)
      (json-object-property-set! json-object property-name json-object-array))))

;; formats a json response
(define (json-format-response response json-format-response-procedure)
  (with-new-json-object
    (lambda (json-object)
      (json-format-response-procedure response json-object)
      (json-object->string json-object))))
