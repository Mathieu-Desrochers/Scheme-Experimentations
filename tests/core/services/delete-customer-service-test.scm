
(declare (unit delete-customer-service-test))

(declare (uses delete-customer-service))
(declare (uses new-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definition

(define-test
  delete-customer-service-test

  (new-customer-service
    (make-new-customer-request
      "Alice"
      "Allisson"
      (date-now)
      (list
        (make-new-customer-shipping-address-subrequest
          "123 Sunny Street"
          "Miami"
          "Florida")))
    (make-new-customer-response
      1))

  (delete-customer-service
    (make-delete-customer-request
      1)
    (make-delete-customer-response)))
