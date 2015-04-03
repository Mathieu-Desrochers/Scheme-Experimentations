
(declare (unit update-customer-service-test))

(declare (uses new-customer-service))
(declare (uses update-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definition

(define-test
  update-customer-service-test

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

  (update-customer-service
    (make-update-customer-request
      1
      "Bob"
      "Bobson"
      (date-now)
      (list
        (make-update-customer-shipping-address-subrequest
          1
          "123 Sunny Street"
          "Miami"
          "Florida")
        (make-update-customer-shipping-address-subrequest
          #f
          "456 Cloudy Boulevard"
          "Seattle"
          "Washington")))
    (make-update-customer-response)))
