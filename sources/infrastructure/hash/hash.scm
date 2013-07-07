
(use srfi-1)
(use srfi-69)

(declare (unit hash))

;; hashes a set of elements according to their key
;; returns a hash table of lists, such as:
;;  [1]: '(alice bob)
;;  [2]: '(carl)
(define (hash-elements elements element-key-procedure element-value-procedure)
  (let ((elements-hash-table (make-hash-table = number-hash)))
    (map
      (lambda (element)
        (let ((element-id (element-key-procedure element))
              (element-value (element-value-procedure element)))
          (hash-table-update!/default
            elements-hash-table
            element-id
            (lambda (elements-list) (append (list element-value) elements-list))
            (list))))
      elements)
    elements-hash-table))
