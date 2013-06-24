
(use srfi-13)

(declare (unit http))

(declare (uses http-intern))
(declare (uses services))

;; encapsulates a http binding
(define-record http-binding method route service parse-request-procedure format-response-procedure)

;; use the application's http bindings
(declare (uses delete-customer-service-http-binding))
(declare (uses get-customer-service-http-binding))
(declare (uses get-shipping-addresses-service-http-binding))
(declare (uses new-customer-service-http-binding))
(declare (uses new-shipping-address-service-http-binding))
(declare (uses update-shipping-address-service-http-binding))

;; makes the application's http bindings
(define (make-http-bindings)
  (list
    (make-delete-customer-service-http-binding)
    (make-get-customer-service-http-binding)
    (make-get-shipping-addresses-service-http-binding)
    (make-new-customer-service-http-binding)
    (make-new-shipping-address-service-http-binding)
    (make-update-shipping-address-service-http-binding)))

;; handles a http request
(define (http-handle-request fastcgi-request*)

  ;; get the environment pointers
  (let ((fastcgi-environment* (fastcgi-request-environment fastcgi-request*))
        (fastcgi-input-stream* (fastcgi-request-input-stream fastcgi-request*))
        (fastcgi-output-stream* (fastcgi-request-output-stream fastcgi-request*)))

    ;; search a http binding match
    ;; for the method and route
    (let* ((method (http-request-method fastcgi-environment*))
           (route (http-request-route fastcgi-environment*))
           (http-binding-match (search-http-binding-match method route)))
      (if (not http-binding-match)
        (http-send-404-not-found fastcgi-output-stream*)

        ;; try to parse the request
        (let* ((route-captures (http-binding-match-route-captures http-binding-match))
               (parse-request-procedure (http-binding-match-parse-request-procedure http-binding-match))
               (request-body (http-read-fastcgi-stream fastcgi-input-stream*))
               (request (parse-request-procedure route-captures request-body)))
          (if (not request)
            (http-send-400-bad-request fastcgi-output-stream*)

            ;; invoke the service
            (let ((service (http-binding-match-service http-binding-match)))
              (invoke-service service request

                ;; send the response
                (lambda (response)
                  (let* ((format-response-procedure (http-binding-match-format-response-procedure http-binding-match))
                         (response-body (format-response-procedure response)))
                    (if (is-empty-json-object-string? response-body)
                      (http-send-204-no-content fastcgi-output-stream*)
                      (http-send-200-ok response-body fastcgi-output-stream*))))

                ;; send the validation errors
                (lambda (validation-errors)
                  (http-send-422-unprocessable-entity validation-errors fastcgi-output-stream*))))))))))
