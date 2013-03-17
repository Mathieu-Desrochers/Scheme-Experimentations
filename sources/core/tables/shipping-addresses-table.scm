
(declare (unit shipping-addresses-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table definition

(define-table
  (shipping-addresses-table
    "shipping-addresses")
  (shipping-address-row
    "shipping-address-id"
    "customer-id"
    "effective-date"
    "street"
    "city"
    "state"))
