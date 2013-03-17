
(declare (unit customers-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table definition

(define-table
  (customers-table
    "customers")
  (customer-row
    "customer-id"
    "first-name"
    "last-name"
    "is-vip"))
