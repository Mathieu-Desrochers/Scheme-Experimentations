
(declare (unit http-bindings))

(declare (uses new-customer-service-http-binding))
(declare (uses new-shipping-address-service-http-binding))

;; encapsulates a http binding
(define-record http-binding method route service parse-request-procedure format-response-procedure)

;; registers the application's http bindings
(define (register-http-bindings)
  (list
    (make-new-customer-service-http-binding)
    (make-new-shipping-address-service-http-binding)))
