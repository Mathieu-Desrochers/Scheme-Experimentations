
(declare (unit get-customer-service-http-binding))

(declare (uses json-format))
(declare (uses json-parse))
(declare (uses get-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-http-binding
  "GET"
  "^customers/(\\d{1,6})$"
  get-customer-service
  http-parse-get-customer-request
  http-format-get-customer-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http parse logic

(define (http-parse-get-customer-request route-captures request-body)
  (make-get-customer-request
    (string->number (car route-captures))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-get-customer-response response)
  (json-format-response response json-format-get-customer-response))
