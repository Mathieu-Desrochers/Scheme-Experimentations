
(declare (unit update-customer-service-http-binding))

(declare (uses json-format))
(declare (uses json-parse))
(declare (uses update-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-http-binding
  "PUT"
  "^customers$"
  "application/json; charset=utf-8"
  update-customer-service
  http-parse-update-customer-request
  http-format-update-customer-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http parse logic

(define (http-parse-update-customer-request route-captures request-body)
  (json-parse-request request-body json-parse-update-customer-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-update-customer-response response)
  (json-format-response response json-format-update-customer-response))
