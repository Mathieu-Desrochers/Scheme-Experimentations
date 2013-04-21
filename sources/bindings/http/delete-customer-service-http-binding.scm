
(declare (unit delete-customer-service-http-binding))

(declare (uses json-format))
(declare (uses json-parse))
(declare (uses delete-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http binding definition

(define-http-binding
  "DELETE"
  "^customers/(\\d{1,6})$"
  delete-customer-service
  http-parse-delete-customer-request
  http-format-delete-customer-response)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http parse logic

(define (http-parse-delete-customer-request route-captures request-body)
  (make-delete-customer-request
    (string->number (car route-captures))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-delete-customer-response response)
  (json-format-response response json-format-delete-customer-response))
