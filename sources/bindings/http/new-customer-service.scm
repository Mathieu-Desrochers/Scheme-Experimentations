
(declare (unit new-customer-service-http-binding))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service definition

;(define-service new-customer-service
;  "POST"
;  "/new-customer")

;(declare (uses http))

;; returns the service registration
(define (new-customer-service-registration)
  (make-http-service-registration
    "POST"
    "/new-customer"
    new-customer-request-parse
    new-customer-service
    new-customer-response-format))

