
make-http-bindings
------------------
A stub procedure in which to make the http bindings.

    (define (make-http-bindings)
      (list
        (make-http-binding ...)
        (make-http-binding ...)
        (make-http-binding ...)))

http-binding
------------
A record that binds http requests to a service.  
See the define-http-binding macro, which automatically  
generates the required make-http-binding procedure for a service.

http-handle-request
-------------------
Handles a http request.

This procedure invokes the make-http-bindings procedure, and looks for a match.  
If none is found, it writes a 404 response.

    (make-http-bindings)

It then invokes the parse-request-procedure of the matched http-binding.  
Should the parse procedure return #f, it writes a 400 response. 

    (http-parse-get-customer-request ...)

It then invokes the service.  
If a validation exception is raised, it writes a 422 response.  
The response body will then contain an array of validation error symbols.  
See also validation-exception? and validation-errors.

    (get-customer-service ...)

If the service succeeds, this procedure invokes the  
format-response-procedure of the matched http-binding.  
It then writes a 200 response with that body content.

    (http-format-get-customer-response ...)

__fastcgi-request*__  
A pointer to a fast cgi request.

__side effect__  
Writes a response to the fast cgi output stream.
