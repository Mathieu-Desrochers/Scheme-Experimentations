
(declare (unit new-shipping-address-service-test))

(declare (uses date-time))
(declare (uses new-customer-service))
(declare (uses new-shipping-address-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definition

(define-test
  new-shipping-address-service-test

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

  (new-shipping-address-service
    (make-new-shipping-address-request
      1
      (date-now)
      "456 Cloudy Boulevard"
      "Seattle"
      "Washington")
    (make-new-shipping-address-response
      2)))
