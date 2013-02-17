
(declare (unit fastcgi))

(foreign-declare "

#include <fcgi_config.h>
#include <fcgi_stdio.h>

// allocates a buffer on the heap
// do not return the buffer as a string pointer
// scheme would then take its own copy of the string
// and the buffer pointer would be dropped
void* FCGX_malloc_buffer(int size)
{
  void* buffer_pointer = malloc(size * sizeof(char));
  return buffer_pointer;
}

// returns the string contained in a buffer
char* FCGX_buffer_string(void* buffer_pointer)
{
  return (char*)buffer_pointer;
}

// frees the specified buffer
void FCGX_free_buffer(void* buffer_pointer)
{
  free(buffer_pointer);
}

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

// extension to the foreign function
// accomodates the void buffer pointers
void* FCGX_GetLineEx(void* str, int n, FCGX_Stream* stream)
{
  return FCGX_GetLine((char*)str, n, stream);
}

")

;; buffers memory management
(define malloc-fastcgi-buffer (foreign-lambda c-pointer "FCGX_malloc_buffer" int))
(define fastcgi-buffer-string (foreign-lambda c-string "FCGX_buffer_string" c-pointer))
(define free-fastcgi-buffer (foreign-lambda void "FCGX_free_buffer" c-pointer))

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

;; reads up to n-1 consecutive bytes from the input stream
(define fastcgi-getline (foreign-lambda c-pointer "FCGX_GetLineEx" c-pointer int fastcgi-stream*))

;; writes a null-terminated character string to the output stream
(define fastcgi-puts (foreign-lambda int "FCGX_PutS" (const c-string) fastcgi-stream*))
