
(declare (uses json))

(with-parsed-json-object "{\"name\":\"value\"}"
  (lambda (json-object)
    (let ((value (json-property-value json-object "name")))
      (display value))))
