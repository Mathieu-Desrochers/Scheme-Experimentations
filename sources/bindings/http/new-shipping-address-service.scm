
(declare (unit new-shipping-address-service-http-binding))

(declare (uses json))
(declare (uses new-shipping-address-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-http-binding
  "POST"
  "^customers/(\\d{1,6})/shipping-addresses$"
  new-shipping-address-service
  http-parse-new-shipping-address-request
  http-format-new-shipping-address-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http parse logic

(define (http-parse-new-shipping-address-request route-captures request-body)
  (let ((new-shipping-address-request (http-parse-json request-body json-parse-new-shipping-address-request)))
    (if new-shipping-address-request
      (let ((customer-id (string->number (car route-captures))))
        (new-shipping-address-request-customer-id-set! new-shipping-address-request customer-id)
        new-shipping-address-request)
      #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-new-shipping-address-response response)
  (with-new-json-object
    (lambda (json-object)
      (json-format-new-shipping-address-response response json-object)
      (json-object->string json-object))))
