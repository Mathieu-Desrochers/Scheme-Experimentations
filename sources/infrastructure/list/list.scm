
(use data-structures)
(use srfi-1)
(use srfi-69)

(declare (unit list))

;; returns the index of the duplicates in a list
;; elements with a false value are ignored
(define (list-duplicates-index list element-value-procedure)

  ;; get the elements value
  (let ((elements-value (map element-value-procedure list))
        (elements-value-count-hash-table (make-hash-table = number-hash)))

    ;; count the elements value
    (map
      (lambda (element-value)
        (when element-value
          (hash-table-update!
            elements-value-count-hash-table
            element-value
            (lambda (element-value-count) (+ element-value-count 1))
            (lambda () 0))))
      elements-value)

    ;; return the index of the elements value
    ;; that were counted more than once
    (filter-map
      (lambda (element-value-with-index)
        (let ((element-value (car element-value-with-index))
              (element-index (cadr element-value-with-index)))
          (if element-value
            (if (> (hash-table-ref elements-value-count-hash-table element-value) 1)
              element-index
              #f)
            #f)))
      (zip elements-value (iota (length elements-value))))))

;; returns the index of the differences between two lists
;; elements with a false value are ignored
(define (list-differences-index
          tested-list
          tested-element-value-procedure
          reference-list
          reference-element-value-procedure)

  ;; get the elements value
  (let ((tested-elements-value (map tested-element-value-procedure tested-list))
        (reference-elements-value (map reference-element-value-procedure reference-list))
        (reference-elements-value-hash-table (make-hash-table = number-hash)))

    ;; hash the reference elements value
    (map
      (lambda (reference-element-value)
        (when reference-element-value
          (hash-table-set!
            reference-elements-value-hash-table
            reference-element-value
            #t)))
      reference-elements-value)

    ;; return the index of the tested elements value
    ;; that are not among the reference elements value
    (filter-map
      (lambda (tested-element-value-with-index)
        (let ((tested-element-value (car tested-element-value-with-index))
              (tested-element-index (cadr tested-element-value-with-index)))
          (if tested-element-value
            (if (hash-table-ref/default reference-elements-value-hash-table tested-element-value #f)
              #f
              tested-element-index)
            #f)))
      (zip tested-elements-value (iota (length tested-elements-value))))))
