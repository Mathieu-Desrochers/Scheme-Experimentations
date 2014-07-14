
(declare (unit update-shipping-address-service-http-binding))

(declare (uses json-format))
(declare (uses json-parse))
(declare (uses update-shipping-address-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-http-binding
  "PUT"
  "^customers/(\\d{1,6})/shipping-addresses/(\\d{1,6})$"
  "application/json; charset=utf-8"
  update-shipping-address-service
  http-parse-update-shipping-address-request
  http-format-update-shipping-address-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http parse logic

(define (http-parse-update-shipping-address-request route-captures request-body)
  (let ((parsed-update-shipping-address-request (json-parse-request request-body json-parse-update-shipping-address-request)))
    (if parsed-update-shipping-address-request
      (make-update-shipping-address-request
        (string->number (list-ref route-captures 0))
        (string->number (list-ref route-captures 1))
        (update-shipping-address-request-effective-date parsed-update-shipping-address-request)
        (update-shipping-address-request-street parsed-update-shipping-address-request)
        (update-shipping-address-request-city parsed-update-shipping-address-request)
        (update-shipping-address-request-state parsed-update-shipping-address-request))
      #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-update-shipping-address-response response)
  (json-format-response response json-format-update-shipping-address-response))
