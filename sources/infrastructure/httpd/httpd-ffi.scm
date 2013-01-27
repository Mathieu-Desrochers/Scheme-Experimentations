
(use srfi-18)

(foreign-declare "

#include <httpd.h>
#include <http_core.h>
#include <http_protocol.h>
#include <http_request.h>

")

;; httpd-request-rec pointers definitions
(define-foreign-type httpd-request-rec "request_rec")
(define-foreign-type httpd-request-rec* (c-pointer "request_rec"))

;; handles a http request
(define-external (httpd_handle_request (httpd-request-rec* request)) int
  (+ 40 2))

;; enslaves the invoking thread to httpd
(define httpd-enslave-scheme-thread (foreign-safe-lambda void "httpd_enslave_scheme_thread"))

;; enslave a thread to httpd
(thread-start! (make-thread httpd-enslave-scheme-thread))

;; give the control back to httpd
(return-to-host)
