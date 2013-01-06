
(declare (unit json-format))
(declare (uses json))

;; formats a value
(define (json-format-value json-object property-name value)
  (with-new-json-object-from-value value
    (lambda (json-object-value)
      (json-object-property-set! json-object property-name json-object-value))))
