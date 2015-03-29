
(declare (unit get-customers-service-http-binding))

(declare (uses json-format))
(declare (uses json-parse))
(declare (uses get-customers-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-http-binding
  "GET"
  "^customers$"
  "application/json; charset=utf-8"
  get-customers-service
  http-parse-get-customers-request
  http-format-get-customers-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http parse logic

(define (http-parse-get-customers-request route-captures request-body)
  (make-get-customers-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-get-customers-response response)
  (json-format-response response json-format-get-customers-response))
