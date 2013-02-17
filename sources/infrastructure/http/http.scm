
(declare (unit http))
(declare (uses fastcgi))
(declare (uses new-customer-service))

;; encapsulates a http registration
(define-record http-registration method route execute parse format)

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
        (let* ((fastcgi-getstr-result (fastcgi-getstr fastcgi-buffer* 5000 fastcgi-stream*))
               (content-read (fastcgi-buffer-string fastcgi-buffer*))
               (content (string-append accumulated-content content-read)))
          (if (< fastcgi-getstr-result 5000)
            content
            (read-fastcgi-stream-iter content))))
      (read-fastcgi-stream-iter ""))))

;; encapsulates a http request
(define-record http-request fastcgi-request*)

;; handles a http request
(define (http-handle-request fastcgi-request*)
  (let* ((fastcgi-environment* (fastcgi-request-environment fastcgi-request*))
         (fastcgi-input-stream* (fastcgi-request-input-stream fastcgi-request*))
         (fastcgi-input (read-fastcgi-stream fastcgi-input-stream*))
         (fastcgi-output-stream* (fastcgi-request-output-stream fastcgi-request*))
         (request-method (fastcgi-getparam "REQUEST_METHOD" fastcgi-environment*))
         (request-uri (fastcgi-getparam "REQUEST_URI" fastcgi-environment*)))
    (fastcgi-puts "Content-Type: text/html; charset=utf-8\r\n\r\n" fastcgi-output-stream*)
    (fastcgi-puts "<html><body><pre>" fastcgi-output-stream*)
    (fastcgi-puts fastcgi-input fastcgi-output-stream*)
    (fastcgi-puts "</pre></body></html>" fastcgi-output-stream*)))
