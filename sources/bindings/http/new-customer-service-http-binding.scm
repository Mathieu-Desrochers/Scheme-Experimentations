
(declare (unit new-customer-service-http-binding))

(declare (uses json-format))
(declare (uses json-parse))
(declare (uses new-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-http-binding
  "POST"
  "^customers$"
  new-customer-service
  http-parse-new-customer-request
  http-format-new-customer-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http parse logic

(define (http-parse-new-customer-request route-captures request-body)
  (json-parse-request request-body json-parse-new-customer-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-new-customer-response response)
  (json-format-response response json-format-new-customer-response))
