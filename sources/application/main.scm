
(require-extension utf8)

(use srfi-1)

(declare (uses json))
(declare (uses new-customer-service))

(with-new-json-object
  (lambda (json-object)
    (format-new-product-response
      (make-new-product-response
        (list
          (make-new-product-supplier-subresponse "Supplier1" 100.00)
          (make-new-product-supplier-subresponse "Supplier2" 200.00)))
      json-object)
    (display
      (json-object->string
        json-object))))
