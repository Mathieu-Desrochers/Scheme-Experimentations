
(declare (unit http-bindings))

(declare (uses delete-customer-service-http-binding))
(declare (uses get-customer-service-http-binding))
(declare (uses get-shipping-addresses-service-http-binding))
(declare (uses new-customer-service-http-binding))
(declare (uses new-shipping-address-service-http-binding))
(declare (uses update-shipping-address-service-http-binding))

;; encapsulates a http binding
(define-record http-binding method route service parse-request-procedure format-response-procedure)

;; registers the application's http bindings
(define (register-http-bindings)
  (list
    (make-delete-customer-service-http-binding)
    (make-get-customer-service-http-binding)
    (make-get-shipping-addresses-service-http-binding)
    (make-new-customer-service-http-binding)
    (make-new-shipping-address-service-http-binding)
    (make-update-shipping-address-service-http-binding)))
