
Tests
-----

__Definition__

    (define-test
      get-customer-service-test

      (new-customer-service
        (make-new-customer-request
          "Alice"
          "Allisson"
          #t)
        (make-new-customer-response
          1))

      (get-customer-service
        (make-get-customer-request
          1)
        (make-get-customer-response
          1
          "Alice"
          "Allisson"
          #t)))

- Defines a test:
 - That is named get-customer-service-test
 - That is composed of two steps

- On the first step:
 - The new-customer-service is invoked with the specified new-customer-request
 - The result is validated against the specified new-customer-response

- On the second step:
 - The get-customer-service is invoked with the specified get-customer-request
 - The result is validated against the specified get-customer-response

- Then one of the following is printed to the console:
 - get-customer-service-test... passed
 - get-customer-service-test... failed

