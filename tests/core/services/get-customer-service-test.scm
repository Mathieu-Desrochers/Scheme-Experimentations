
(declare (unit get-customer-service-test))

(declare (uses get-customer-service))
(declare (uses new-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definition

(define-test
  get-customer-service-test

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

  (get-customer-service
    (make-get-customer-request
      1)
    (make-get-customer-response
      1
      "Alice"
      "Allisson"
      (date-now)
      (list
        (make-get-customer-shipping-address-subresponse
          1
          "123 Sunny Street"
          "Miami"
          "Florida")))))
