
(require-extension utf8)

(use srfi-1)

(declare (uses json))
(declare (uses new-customer-service))

(with-new-json-object
  (lambda (json-object)
    (format-get-product-response
      (make-get-product-response '(10 11 12 13 14) '("Green" "Yellow" "Blue"))
      json-object)
    (display
      (json-object->string
        json-object))))
