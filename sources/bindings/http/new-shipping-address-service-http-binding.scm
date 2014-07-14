
(declare (unit new-shipping-address-service-http-binding))

(declare (uses json-format))
(declare (uses json-parse))
(declare (uses new-shipping-address-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-http-binding
  "POST"
  "^customers/(\\d{1,6})/shipping-addresses$"
  "application/json; charset=utf-8"
  new-shipping-address-service
  http-parse-new-shipping-address-request
  http-format-new-shipping-address-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http parse logic

(define (http-parse-new-shipping-address-request route-captures request-body)
  (let ((parsed-new-shipping-address-request (json-parse-request request-body json-parse-new-shipping-address-request)))
    (if parsed-new-shipping-address-request
      (make-new-shipping-address-request
        (string->number (car route-captures))
        (new-shipping-address-request-effective-date parsed-new-shipping-address-request)
        (new-shipping-address-request-street parsed-new-shipping-address-request)
        (new-shipping-address-request-city parsed-new-shipping-address-request)
        (new-shipping-address-request-state parsed-new-shipping-address-request))
      #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-new-shipping-address-response response)
  (json-format-response response json-format-new-shipping-address-response))
