
(declare (unit http))
(declare (uses fastcgi))
(declare (uses new-customer-service))

;; encapsulates a http registration
(define-record http-registration method route execute parse format)

;; encapsulates a http request
(define-record http-request fastcgi-request*)

;; returns the http registrations
(define (http-registrations)
  (list
    (new-customer-service-http-registration)))

;; searches the http registration matching a method and route
(define (search-http-registration method route)
  (define (http-registration-match? http-registration)
    (and (eq? (http-registration-method http-registration) method)
         (eq? (http-registration-route http-registration) route)))
  (define (search-http-registration-iter http-registrations)
    (if (null? http-registrations)
      #f
      (let ((http-registration (car http-registrations)))
        (if (http-registration-match? http-registration)
          http-registration
          (search-http-registration-iter
            (cdr http-registrations))))))
  (search-http-registration-iter
    (http-registrations)))

;; handles a http request
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
