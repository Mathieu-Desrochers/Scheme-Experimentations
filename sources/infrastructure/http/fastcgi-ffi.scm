
(declare (unit fastcgi-ffi))

(foreign-declare "

#include <fcgi_config.h>
#include <fcgi_stdio.h>

// returns the output stream of a request
FCGX_Stream* FCGX_OutputStream(FCGX_Request* request)
{
  FCGX_Stream* stream = request->out;
  return stream;
}

")

;; fastcgi-request pointers definitions
(define-foreign-type fastcgi-request "FCGX_Request")
(define-foreign-type fastcgi-request* (c-pointer fastcgi-request))

;; fastcgi-stream pointers definitions
(define-foreign-type fastcgi-stream "FCGX_Stream")
(define-foreign-type fastcgi-stream* (c-pointer fastcgi-stream))

;; returns the output stream of a request
(define fastcgi-request-output-stream (foreign-lambda fastcgi-stream* "FCGX_OutputStream" fastcgi-request*))

;; writes a null-terminated character string to the output stream
(define fastcgi-puts (foreign-lambda int "FCGX_PutS" (const c-string) fastcgi-stream*))
