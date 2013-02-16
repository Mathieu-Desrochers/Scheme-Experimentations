
(declare (unit http))
(declare (uses fastcgi))
(declare (uses new-customer-service))

;; encapsulates a service registration
(define-record http-service-registration method route parse execute format)

;; returns the service registrations
(define (http-service-registrations)
  (list
    (new-customer-service-registration)))

;; searches the service registration matching a method and route
(define (search-http-service-registration route)
  (define (search-http-service-registration-iter http-service-registrations)
    (if (null? http-service-registrations)
      #f
      (let ((http-service-registration (car http-service-registrations)))
        (if (eq? (http-service-registration-route http-service-registration) route)
          http-service-registration
          (search-http-service-registration-iter
            (cdr http-service-registrations))))))
  (search-http-service-registration-iter
    (http-service-registrations)))

;; handles an http request
(define (http-handle-request fastcgi-request*)
  (let* ((fastcgi-environment* (fastcgi-request-environment fastcgi-request*))
         (fastcgi-input-stream* (fastcgi-request-input-stream fastcgi-request*))
         (fastcgi-output-stream* (fastcgi-request-output-stream fastcgi-request*))
         (request-method (fastcgi-getparam "REQUEST_METHOD" fastcgi-environment*))
         (request-uri (fastcgi-getparam "REQUEST_URI" fastcgi-environment*)))
    (fastcgi-puts "Content-Type: text/html; charset=utf-8\r\n\r\n" fastcgi-output-stream*)
    (fastcgi-puts "Hello from scheme!<br>" fastcgi-output-stream*)
    (fastcgi-puts request-method fastcgi-output-stream*)
    (fastcgi-puts "<br>" fastcgi-output-stream*)
    (fastcgi-puts request-uri fastcgi-output-stream*)))
