
(use srfi-1)
(use srfi-69)

(declare (unit hash))

;; hashes elements according to their key
;; elements are required to have unique keys
;; returns a hash table such as:
;;  [1]: alice
;;  [2]: bob
(define (hash-with-unique-keys elements element-key-procedure element-value-procedure)
  (let ((elements-hash-table (make-hash-table = number-hash)))
    (map
      (lambda (element)
        (let ((element-id (element-key-procedure element))
              (element-value (element-value-procedure element)))
          (hash-table-set!
            elements-hash-table
            element-id
            element-value)))
      elements)
    elements-hash-table))

;; hashes elements according to their key
;; elements are permitted to share keys
;; returns a hash table such as:
;;  [1]: '(alice bob)
;;  [2]: '(carl)
(define (hash-with-shared-keys elements element-key-procedure element-value-procedure)
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
