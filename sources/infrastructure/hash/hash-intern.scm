
(use srfi-1)
(use srfi-69)

(declare (unit hash-intern))

;; hashes elements according to their unique key
(define (hash-with-unique-keys
          elements
          element-key-procedure
          element-value-procedure
          element-equal?
          element-hash)

  ;; make the hash table
  (let ((elements-hash-table
          (make-hash-table
            element-equal?
            element-hash)))

    ;; hash the elements
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

;; hashes elements according to their shared key
(define (hash-with-shared-keys
          elements
          element-key-procedure
          element-value-procedure
          element-equal?
          element-hash)

  ;; make the hash table
  (let ((elements-hash-table
          (make-hash-table
            element-equal?
            element-hash)))

    ;; hash the elements
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
