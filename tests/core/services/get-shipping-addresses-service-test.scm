
(declare (unit get-shipping-addresses-service-test))

(declare (uses date-time))
(declare (uses get-shipping-addresses-service))
(declare (uses new-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definition

(define-test
  get-shipping-addresses-service-test

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

  (get-shipping-addresses-service
    (make-get-shipping-addresses-request
      1)
    (make-get-shipping-addresses-response
      (list
        (make-get-shipping-addresses-shipping-address-subresponse
          1
          (date-now)
          "123 Sunny Street"
          "Miami"
          "Florida")))))
