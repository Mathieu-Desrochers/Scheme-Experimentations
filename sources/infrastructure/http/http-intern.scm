
(use srfi-13)

(declare (unit http-intern))

(declare (uses fastcgi))
(declare (uses json))
(declare (uses regex))

;; returns the method of a http request
(define (http-request-method fastcgi-environment*)
  (fastcgi-getparam "REQUEST_METHOD" fastcgi-environment*))

;; returns the route of a http request
(define (http-request-route fastcgi-environment*)
  (let ((context-prefix (fastcgi-getparam "CONTEXT_PREFIX" fastcgi-environment*))
        (request-uri (fastcgi-getparam "REQUEST_URI" fastcgi-environment*)))
    (string-drop request-uri (string-length context-prefix))))

;; encapsulates a http binding match
(define-record
  http-binding-match
  method
  route
  route-captures
  content-type
  service
  parse-request-procedure
  format-response-procedure)

;; returns whether a http binding
;; matches a method and a route
(define (http-binding-match? http-binding method route)
  (if (equal? (http-binding-method http-binding) method)
    (let ((http-binding-route-regex-match (regex-match (http-binding-route http-binding) route)))
      (if (not (null? http-binding-route-regex-match))
        (let ((route-captures (cdr http-binding-route-regex-match)))
          (make-http-binding-match
            (http-binding-method http-binding)
            (http-binding-route http-binding)
            route-captures
            (http-binding-content-type http-binding)
            (http-binding-service http-binding)
            (http-binding-parse-request-procedure http-binding)
            (http-binding-format-response-procedure http-binding)))
        #f))
    #f))

;; iterates through the http bindings,
;; searching for one that matches a method and a route
(define (search-http-binding-match method route)
  (define (search-http-binding-match-iter http-bindings)
    (if (not (null? http-bindings))
      (let ((http-binding-match (http-binding-match? (car http-bindings) method route)))
        (if http-binding-match
          http-binding-match
          (search-http-binding-match-iter
            (cdr http-bindings))))
      #f))
  (search-http-binding-match-iter
    (make-http-bindings)))

;; invokes a procedure with a fastcgi buffer
(define (http-with-fastcgi-buffer* buffer-size procedure)
  (define (checked-malloc-fastcgi-buffer)
    (let ((fastcgi-buffer* (malloc-fastcgi-buffer buffer-size)))
      (when (not fastcgi-buffer*)
        (abort "failed to allocate fastcgi buffer"))
      fastcgi-buffer*))
  (with-guaranteed-release
    checked-malloc-fastcgi-buffer
    procedure
    free-fastcgi-buffer))

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
  (cond ((string? body) (fastcgi-puts (string-append body "\r\n") fastcgi-output-stream*))
        ((blob? body) (fastcgi-putstr body (blob-size body) fastcgi-output-stream*))
        (else (abort "unsupported body type"))))

;; sends a 200 ok
(define (http-send-200-ok content-type response-body fastcgi-output-stream*)
  (http-write-header "Status: 200 OK" fastcgi-output-stream*)
  (http-write-header (string-append "Content-Type: " content-type) fastcgi-output-stream*)
  (http-close-headers fastcgi-output-stream*)
  (http-write-body response-body fastcgi-output-stream*))

;; sends a 204 no content
(define (http-send-204-no-content fastcgi-output-stream*)
  (http-write-header "Status: 204 No Content" fastcgi-output-stream*)
  (http-close-headers fastcgi-output-stream*))

;; sends a 400 bad request error
(define (http-send-400-bad-request fastcgi-output-stream*)
  (http-write-header "Status: 400 Bad Request" fastcgi-output-stream*)
  (http-close-headers fastcgi-output-stream*))

;; sends a 404 not found error
(define (http-send-404-not-found fastcgi-output-stream*)
  (http-write-header "Status: 404 Not Found" fastcgi-output-stream*)
  (http-close-headers fastcgi-output-stream*))

;; sends a 422 unprocessable entity error
(define (http-send-422-unprocessable-entity validation-errors fastcgi-output-stream*)

  ;; write the response headers
  (http-write-header "Status: 422 Unprocessable Entity" fastcgi-output-stream*)
  (http-write-header "Content-Type: application/json; charset=utf-8" fastcgi-output-stream*)
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
