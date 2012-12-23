
(declare (unit customer-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table definition

(define-table
  customer-table
  "customer"
  customer-row
  ("customer-id"
   "first-name"
   "last-name"))
