
(use srfi-13)

(declare (unit http))
(declare (uses fastcgi))
(declare (uses new-customer-service-http-binding))

;; encapsulates a http registration
(define-record http-registration method route execute parse format)

;; returns the http registrations
(define (http-registrations)
  (list
    (new-customer-service-http-registration)))

;; returns the method of a http request
(define (http-request-method fastcgi-environment*)
  (fastcgi-getparam "REQUEST_METHOD" fastcgi-environment*))

;; returns the route of a http request
(define (http-request-route fastcgi-environment*)
  (let ((context-prefix (fastcgi-getparam "CONTEXT_PREFIX" fastcgi-environment*))
        (request-uri (fastcgi-getparam "REQUEST_URI" fastcgi-environment*)))
    (string-drop request-uri (string-length context-prefix))))

;; searches the http registration matching a method and route
(define (search-http-registration method route)
  (define (http-registration-match? http-registration)
    (and (equal? (http-registration-method http-registration) method)
         (equal? (http-registration-route http-registration) route)))
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

;; invokes a procedure with a fastcgi buffer
(define (with-fastcgi-buffer* procedure)
  (let ((fastcgi-buffer* (malloc-fastcgi-buffer 5000)))
    (when (not fastcgi-buffer*)
      (abort "could not allocate fastcgi buffer"))
    (handle-exceptions exception
      (begin
        (free-fastcgi-buffer fastcgi-buffer*)
        (abort exception))
      (let ((procedure-result (procedure fastcgi-buffer*)))
        (free-fastcgi-buffer fastcgi-buffer*)
        procedure-result))))

;; reads a fastcgi stream
(define (read-fastcgi-stream fastcgi-stream*)
  (with-fastcgi-buffer*
    (lambda (fastcgi-buffer*)
      (define (read-fastcgi-stream-iter accumulated-content)
        (let ((fastcgi-getline-result (fastcgi-getline fastcgi-buffer* 5000 fastcgi-stream*)))
          (if (not fastcgi-getline-result)
            accumulated-content
            (let* ((content-read (fastcgi-buffer-string fastcgi-buffer*))
                   (content (string-append accumulated-content content-read)))
              (read-fastcgi-stream-iter content)))))
      (read-fastcgi-stream-iter ""))))

;; writes a http header
(define (write-http-header header fastcgi-output-stream*)
  (let ((header-line (string-append header "\r\n")))
    (fastcgi-puts header-line fastcgi-output-stream*)))

;; closes the http headers
(define (close-http-headers fastcgi-output-stream*)
    (fastcgi-puts "\r\n" fastcgi-output-stream*))

;; sends a 404 error
(define (send-404 fastcgi-output-stream*)
  (write-http-header "Status: 404 Not Found" fastcgi-output-stream*)
  (write-http-header "Content-Type: text/plain; charset=utf-8" fastcgi-output-stream*)
  (close-http-headers fastcgi-output-stream*))

;; handles a http request
(define (http-handle-request fastcgi-request*)
  (let ((fastcgi-output-stream* (fastcgi-request-output-stream fastcgi-request*)))
    (let* ((fastcgi-environment* (fastcgi-request-environment fastcgi-request*))
           (method (http-request-method fastcgi-environment*))
           (route (http-request-route fastcgi-environment*))
           (http-registration (search-http-registration method route)))
      (if (not http-registration)
        (send-404 fastcgi-output-stream*)
        (let* ((fastcgi-input-stream* (fastcgi-request-input-stream fastcgi-request*))
               (fastcgi-input (read-fastcgi-stream fastcgi-input-stream*)))
            (fastcgi-puts "Content-Type: text/html; charset=utf-8\r\n\r\n" fastcgi-output-stream*)
            (fastcgi-puts "<html><body><pre>" fastcgi-output-stream*)
            (fastcgi-puts fastcgi-input fastcgi-output-stream*)
            (fastcgi-puts "</pre></body></html>" fastcgi-output-stream*)
            (fastcgi-puts "\r\n" fastcgi-output-stream*))))))
