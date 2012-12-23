
(declare (unit customer-address-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table definition

(define-table
  customer-address-table
  "customer-address"
  customer-address-row
  ("customer-address-id"
   "customer-id"
   "address"
   "city"
   "state"))
