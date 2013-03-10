
(use srfi-13)

(declare (unit http))

(declare (uses exceptions))
(declare (uses fastcgi))
(declare (uses json))
(declare (uses services))

(declare (uses new-customer-service-http-binding))

;; encapsulates a http registration
(define-record http-registration method route service parse-request-procedure format-response-procedure)

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
(define (http-with-fastcgi-buffer* buffer-size procedure)
  (let ((fastcgi-buffer* (malloc-fastcgi-buffer buffer-size)))
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
(define (http-read-fastcgi-stream fastcgi-stream*)
  (let ((buffer-size 5000))
    (http-with-fastcgi-buffer*
      buffer-size
      (lambda (fastcgi-buffer*)
        (define (http-read-fastcgi-stream-iter accumulated-content)
          (let ((fastcgi-getline-result (fastcgi-getline fastcgi-buffer* buffer-size fastcgi-stream*)))
            (if (not fastcgi-getline-result)
              accumulated-content
              (let* ((content-read (fastcgi-buffer-string fastcgi-buffer*))
                     (content (string-append accumulated-content content-read)))
                (http-read-fastcgi-stream-iter content)))))
        (http-read-fastcgi-stream-iter "")))))

;; writes a http header
(define (http-write-header header fastcgi-output-stream*)
  (let ((header-line (string-append header "\r\n")))
    (fastcgi-puts header-line fastcgi-output-stream*)))

;; closes the http headers
(define (http-close-headers fastcgi-output-stream*)
    (fastcgi-puts "\r\n" fastcgi-output-stream*))

;; writes the http body
(define (http-write-body body fastcgi-output-stream*)
  (fastcgi-puts body fastcgi-output-stream*)
  (fastcgi-puts "\r\n" fastcgi-output-stream*))

;; sends a 200 ok
(define (http-send-200-ok response-body fastcgi-output-stream*)
  (http-write-header "Status: 200 OK" fastcgi-output-stream*)
  (http-write-header "Content-Type: text/json; charset=utf-8" fastcgi-output-stream*)
  (http-close-headers fastcgi-output-stream*)
  (http-write-body response-body fastcgi-output-stream*))

;; sends a 400 bad request error
(define (http-send-400-bad-request fastcgi-output-stream*)
  (http-write-header "Status: 400 Bad Request" fastcgi-output-stream*)
  (http-write-header "Content-Type: text/plain; charset=utf-8" fastcgi-output-stream*)
  (http-close-headers fastcgi-output-stream*))

;; sends a 404 not found error
(define (http-send-404-not-found fastcgi-output-stream*)
  (http-write-header "Status: 404 Not Found" fastcgi-output-stream*)
  (http-write-header "Content-Type: text/plain; charset=utf-8" fastcgi-output-stream*)
  (http-close-headers fastcgi-output-stream*))

;; sends a 422 unprocessable entity error
(define (http-send-422-unprocessable-entity validation-errors fastcgi-output-stream*)
  
  ;; write the response headers
  (http-write-header "Status: 422 Unprocessable Entity" fastcgi-output-stream*)
  (http-write-header "Content-Type: text/json; charset=utf-8" fastcgi-output-stream*)
  (http-close-headers fastcgi-output-stream*)
  
  ;; formats the validation errors into a json array
  (define (format-validation-errors)
    (with-new-json-object
      (lambda (json-object)
        (with-new-json-object-array
          (lambda (json-object-array)
            (map
              (lambda (validation-error)
                (with-new-json-object-from-value (symbol->string validation-error)
                  (lambda (json-object-from-value)
                    (json-object-array-append! json-object-array json-object-from-value))))
              validation-errors)
            (json-object-property-set! json-object "errors" json-object-array)
            (json-object->string json-object))))))

  ;; write the response body
  (let ((response-body (format-validation-errors)))
    (http-write-body response-body fastcgi-output-stream*)))

;; handles a http request
(define (http-handle-request fastcgi-request*)

  ;; get the environment pointers
  (let ((fastcgi-environment* (fastcgi-request-environment fastcgi-request*))
        (fastcgi-input-stream* (fastcgi-request-input-stream fastcgi-request*))
        (fastcgi-output-stream* (fastcgi-request-output-stream fastcgi-request*)))

    ;; search for a http registration matching
    ;; the requested method and route
    (let* ((method (http-request-method fastcgi-environment*))
           (route (http-request-route fastcgi-environment*))
           (http-registration (search-http-registration method route)))
      (if (not http-registration)
        (http-send-404-not-found fastcgi-output-stream*)

        ;; try to parse the request
        (let* ((http-request-body (http-read-fastcgi-stream fastcgi-input-stream*))
               (parse-request-procedure (http-registration-parse-request-procedure http-registration))
               (request (hide-exceptions (lambda () (parse-request-procedure http-request-body)))))
          (if (not request)
            (http-send-400-bad-request fastcgi-output-stream*)
            
            ;; invoke the service
            (let* ((service (http-registration-service http-registration)))
              (invoke-service service request
              
                ;; send the response
                (lambda (response)
                  (let* ((format-response-procedure (http-registration-format-response-procedure http-registration))
                         (http-response-body (format-response-procedure response)))
                    (http-send-200-ok http-response-body fastcgi-output-stream*)))
                
                ;; send the validation errors
                (lambda (validation-errors)
                  (http-send-422-unprocessable-entity validation-errors fastcgi-output-stream*))))))))))
