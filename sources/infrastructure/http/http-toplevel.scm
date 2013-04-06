
(declare (uses http))

;; handles an http request
(define-external (http_handle_request (c-pointer request)) void
  (http-handle-request request))

(return-to-host)
