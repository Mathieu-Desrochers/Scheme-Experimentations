
(declare (unit new-customer-service-http-binding))

(declare (uses json))
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

(define (http-parse-new-customer-request http-request-body)
  (with-parsed-json-object http-request-body
    (lambda (json-object)
      (json-parse-new-customer-request json-object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http format logic

(define (http-format-new-customer-response response)
  (with-new-json-object
    (lambda (json-object)
      (json-format-new-customer-response response json-object)
      (json-object->string json-object))))
