
(declare (unit shipping-addresses-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table definition

(define-table
  (shipping-addresses-table
    "shipping-addresses")
  (shipping-address-row
    ("shipping-address-id" integer)
    ("customer-id" integer)
    ("effective-date" date)
    ("street" string)
    ("city" string)
    ("state" string)))
