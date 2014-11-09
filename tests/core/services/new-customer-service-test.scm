
(declare (unit new-customer-service-test))

(declare (uses new-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definition

(define-test
  new-customer-service-test

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
      1)))
