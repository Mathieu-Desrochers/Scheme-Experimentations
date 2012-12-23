
(declare (uses new-customer-service))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(display
  (validate-new-customer-request
    (make-new-customer-request
      "Mathieu"
      "Desrochers"
      (list
        (make-new-customer-request-address-subrequest "Street" "City" 1)))))
