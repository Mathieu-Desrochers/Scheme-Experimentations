
(declare (uses fastcgi-ffi))

(define-external (http_handle_request (c-pointer request)) void
  (http-handle-request request))

(define (http-handle-request fastcgi-request*)
  (let ((fastcgi-output-stream* (fastcgi-request-output-stream fastcgi-request*)))
    (fastcgi-puts "Content-Type: text/html; charset=utf-8\r\n\r\n" fastcgi-output-stream*)
    (fastcgi-puts "Hello from scheme!" fastcgi-output-stream*)))

(return-to-host)
