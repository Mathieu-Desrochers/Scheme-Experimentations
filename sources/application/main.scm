
(require-extension utf8)

(use srfi-1)

(declare (uses json))
(declare (uses new-customer-service))

(with-new-json-object
  (lambda (json-object)
    (format-get-customer-response
      (make-get-customer-response "Alice" 8 5000.00)
      json-object)
    (display
      (json-object->string
        json-object))))
