
(require-extension utf8)

(use srfi-1)

(declare (uses json))
(declare (uses new-customer-service))

(with-new-json-object
  (lambda (json-object)
    (format-new-customer-response
      (make-new-customer-response 
        (make-new-customer-address-subresponse "123 Sunny Street" "Montreal" "H2J 4R1"))
      json-object)
    (display
      (json-object->string
        json-object))))
