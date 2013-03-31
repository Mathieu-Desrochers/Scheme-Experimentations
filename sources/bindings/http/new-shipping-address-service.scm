
(declare (unit new-shipping-address-service-http-binding))

(declare (uses json))
(declare (uses new-shipping-address-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-http-binding
  "POST"
  "^customers/(\\d{1,10})/shipping-addresses$"
  new-shipping-address-service
  http-parse-new-shipping-address-request
  http-format-new-shipping-address-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http parse logic

(define (http-parse-new-shipping-address-request http-request-body)
  (with-parsed-json-object http-request-body
    (lambda (json-object)
      (json-parse-new-shipping-address-request json-object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-new-shipping-address-response response)
  (with-new-json-object
    (lambda (json-object)
      (json-format-new-shipping-address-response response json-object)
      (json-object->string json-object))))
