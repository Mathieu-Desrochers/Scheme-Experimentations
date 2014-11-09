
(declare (unit update-shipping-address-service-test))

(declare (uses date-time))
(declare (uses new-customer-service))
(declare (uses update-shipping-address-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definition

(define-test
  update-shipping-address-service-test

  (new-customer-service
    (make-new-customer-request
      "Alice"
      "Allisson"
      #t
      (make-new-customer-shipping-address-subrequest
        "123 Sunny Street"
        "Miami"
        "Florida"))
    (make-new-customer-response
      1))

  (update-shipping-address-service
    (make-update-shipping-address-request
      1
      1
      (date-now)
      "456 Cloudy Boulevard"
      "Seattle"
      "Washington")
    (make-update-shipping-address-response)))
