
(declare (unit get-shipping-addresses-service-http-binding))

(declare (uses json-format))
(declare (uses json-parse))
(declare (uses get-shipping-addresses-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-http-binding
  "GET"
  "^customers/(\\d{1,6})/shipping-addresses$"
  get-shipping-addresses-service
  http-parse-get-shipping-addresses-request
  http-format-get-shipping-addresses-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http parse logic

(define (http-parse-get-shipping-addresses-request route-captures request-body)
  (make-get-shipping-addresses-request
    (string->number (car route-captures))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-get-shipping-addresses-response response)
  (json-format-response response json-format-get-shipping-addresses-response))
