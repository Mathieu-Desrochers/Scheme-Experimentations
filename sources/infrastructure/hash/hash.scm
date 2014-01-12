
(use srfi-1)
(use srfi-69)

(declare (unit hash))

(declare (uses hash-intern))

;; hashes elements according to their numeric key
;; elements are required to have unique keys
;; returns a hash table such as:
;;  [1]: alice
;;  [2]: bob
(define (hash-with-unique-numeric-keys
          elements
          element-key-procedure
          element-value-procedure)

  (hash-with-unique-keys
    elements
    element-key-procedure
    element-value-procedure
    =
    number-hash))

;; hashes elements according to their numeric key
;; elements are permitted to share keys
;; returns a hash table such as:
;;  [1]: '(alice bob)
;;  [2]: '(carl)
(define (hash-with-shared-numeric-keys
          elements
          element-key-procedure
          element-value-procedure)

  (hash-with-shared-keys
    elements
    element-key-procedure
    element-value-procedure
    =
    number-hash))
