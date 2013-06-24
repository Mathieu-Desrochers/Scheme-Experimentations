
invoke-service
--------------
Prepares a database connection and invokes a service.

__service__  
The service procedure.

    new-customer

__request__  
The service request.

    new-customer-request

__success-procedure__  
A procedure invoked when the service succeeds.  
It is provided with the service response.

    (lambda (new-customer-response) ...)

__validation-errors-procedure__  
A procedure invoked when the service throws a validation exception.  
See also validation-exception? and validation-errors.

    (lambda (validation-errors) ...)
