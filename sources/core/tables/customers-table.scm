
(declare (unit customers-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table definition

(define-table
  (customers-table
    "customers")
  (customer-row
    ("customer-id" integer)
    ("first-name" string)
    ("last-name" string)
    ("birthdate" date))
  (custom-selects)
  (custom-single-value-selects)
  (custom-executes))
