
(use data-structures)
(use srfi-1)
(use srfi-69)

(declare (unit list))

(declare (uses list-intern))

;; returns the index of the duplicates in a list
(define (list-duplicates-index elements element-value-procedure)

  ;; get the elements value
  (let ((elements-value (map element-value-procedure elements))
        (elements-value-count-hash-table (make-hash-table = number-hash)))

    ;; count the elements value
    (map
      (lambda (element-value)
        (hash-table-update!
          elements-value-count-hash-table
          element-value
          (lambda (element-value-count) (+ element-value-count 1))
          (lambda () 0)))
      elements-value)

    ;; return the index of the elements value
    ;; that were counted more than once
    (filter-map
      (lambda (element-value-with-index)
        (let ((element-value (car element-value-with-index))
              (element-index (cadr element-value-with-index)))
          (if (> (hash-table-ref elements-value-count-hash-table element-value) 1)
            element-index
            #f)))
      (zip elements-value (iota (length elements-value))))))

;; returns the index of the left elements
;; whose value is found among the right elements
(define (list-matches-index
          left-elements
          left-element-value-procedure
          right-elements
          right-element-value-procedure)

  (list-match-elements-value-index
    left-elements
    left-element-value-procedure
    right-elements
    right-element-value-procedure
    #t
    #f))

;; returns the index of the left elements
;; whose value is not found among the right elements
(define (list-non-matches-index
          left-elements
          left-element-value-procedure
          right-elements
          right-element-value-procedure)

  (list-match-elements-value-index
    left-elements
    left-element-value-procedure
    right-elements
    right-element-value-procedure
    #f
    #t))

;; sorts a list of elements
(define (list-sort elements element-sort-value-procedure)
  (sort
    elements
    (lambda (x y)
      (< (element-sort-value-procedure x)
         (element-sort-value-procedure y)))))
