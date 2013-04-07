
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
    ("state" string))
  (custom-reads
    (shipping-addresses-table-select-effective-by-customer-id
      (string-append
        "SELECT * "
        "FROM \"shipping-addresses\" "
        "WHERE \"customer-id\" = ?1 "
        "AND \"effective-date\" <= ?2 "
        "ORDER BY \"effective-date\" DESC "
        "LIMIT 1;")
      customer-id
      effective-date)))
