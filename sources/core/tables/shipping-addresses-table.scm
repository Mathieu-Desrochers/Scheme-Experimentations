
(declare (unit shipping-addresses-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table definition

(define-table
  (shipping-addresses-table
    "shipping-addresses")
  (shipping-address-row
    ("shipping-address-id" integer)
    ("customer-id" integer)
    ("street" string)
    ("city" string)
    ("state" string))
  (custom-selects
    (shipping-addresses-table-select-by-customer-id
      (string-append
        "SELECT * "
        "FROM \"shipping-addresses\" "
        "WHERE \"customer-id\" = ?1;")
      customer-id))
  (custom-single-value-selects)
  (custom-executes
    (shipping-addresses-table-delete-by-customer-id
      (string-append
        "DELETE "
        "FROM \"shipping-addresses\" "
        "WHERE \"customer-id\" = ?1;")
      customer-id)))
