
(declare (unit fastcgi-ffi))

(foreign-declare "

#include <fcgi_config.h>
#include <fcgi_stdio.h>

// returns the environment of a request
char** FCGX_Environment(FCGX_Request* request)
{
  char** paramArray = request->envp;
  return paramArray;
}

// returns the input stream of a request
FCGX_Stream* FCGX_InputStream(FCGX_Request* request)
{
  FCGX_Stream* stream = request->in;
  return stream;
}

// returns the output stream of a request
FCGX_Stream* FCGX_OutputStream(FCGX_Request* request)
{
  FCGX_Stream* stream = request->out;
  return stream;
}

")

;; fastcgi-environment pointers definitions
(define-foreign-type fastcgi-environment* (c-pointer c-string))

;; fastcgi-request pointers definitions
(define-foreign-type fastcgi-request "FCGX_Request")
(define-foreign-type fastcgi-request* (c-pointer fastcgi-request))

;; fastcgi-stream pointers definitions
(define-foreign-type fastcgi-stream "FCGX_Stream")
(define-foreign-type fastcgi-stream* (c-pointer fastcgi-stream))

;; returns the environment of a request
(define fastcgi-request-environment (foreign-lambda fastcgi-environment* "FCGX_Environment" fastcgi-request*))

;; obtains value of fcgi parameter in environment
(define fastcgi-getparam (foreign-lambda c-string "FCGX_GetParam" (const c-string) fastcgi-environment*))

;; returns the input stream of a request
(define fastcgi-request-input-stream (foreign-lambda fastcgi-stream* "FCGX_InputStream" fastcgi-request*))

;; returns the output stream of a request
(define fastcgi-request-output-stream (foreign-lambda fastcgi-stream* "FCGX_OutputStream" fastcgi-request*))

;; writes a null-terminated character string to the output stream
(define fastcgi-puts (foreign-lambda int "FCGX_PutS" (const c-string) fastcgi-stream*))
