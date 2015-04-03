
(declare (unit get-customers-service-test))

(declare (uses get-customers-service))
(declare (uses new-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definition

(define-test
  get-customers-service-test

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

  (get-customers-service
    (make-get-customers-request)
    (make-get-customers-response
      (list
        (make-get-customers-customer-subresponse
          1
          "Alice"
          "Allisson")))))
