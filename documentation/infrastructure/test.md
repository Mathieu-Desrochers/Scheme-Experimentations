
test-step
---------
Represents a test step.

__service__  
The service to be invoked.

__request__  
The request to send to the service.

__response__  
The expected response.


test-run
--------
Runs a test composed of a serie of test steps.

Every step is run sequentially by invoking its service with the specified request.  
A step is considered to have passed when its service returns the expected response.  
If all the steps pass, the name of the service is printed out with the passed mention.  
Otherwise, the test stops at the faulty step and the failed mention is used.

The test is run inside a transaction that is then rollbacked.

__name__  
The name of the test.

    "new-customer-service"

__test-steps__  
The steps of the test.

    (list
      (make-test-step
        new-customer-service
        (make-new-customer-request
          "Alice"
          "Alisson")
        (make-new-customer-service
          1000))
      (make-test-step
        get-customer-service
        (make-get-customer-request
          1000)
        (make-get-customer-response
          "Alice"
          "Alisson")))

__output__  

    new-customer-service... passed

