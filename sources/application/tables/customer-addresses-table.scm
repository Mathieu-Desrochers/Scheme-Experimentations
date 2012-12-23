
(declare (unit customer-addresses-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table definition

(define-table
  (customer-addresses-table
    "customer-addresses")
  (customer-address-row
    "customer-address-id"
    "customer-id"
    "address"
    "city"
    "state"))
