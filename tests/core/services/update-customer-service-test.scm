
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
      "2001-12-31"
      (make-new-customer-shipping-address-subrequest
        "123 Sunny Street"
        "Miami"
        "Florida"))
    (make-new-customer-response
      1))

  (update-customer-service
    (make-update-customer-request
      1
      "Bob"
      "Bobson"
      "2002-12-31")
    (make-update-customer-response)))
