
(use data-structures)
(use srfi-1)
(use srfi-69)

(declare (unit list-intern))

;; returns the index of the elements
;; that appear more than once in a list
(define (list-duplicates-index-intern
          elements
          element-value-procedure
          element-value-equal?
          element-value-hash)

  ;; get the elements value
  (let ((elements-value (map element-value-procedure elements))
        (elements-value-count-hash-table
          (make-hash-table
            element-value-equal?
            element-value-hash)))

    ;; count the elements value
    (for-each
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

;; returns the index of the elements in a first list
;; whose value can be matched in a second list or not
(define (list-matches-or-non-matches-index-intern
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure
          element-value-equal?
          element-value-hash
          keep-matches-index
          keep-non-matches-index)

  ;; get the first and second elements value
  (let ((first-elements-value (map first-element-value-procedure first-elements))
        (second-elements-value (map second-element-value-procedure second-elements)))

    ;; hash the second elements value
    (let ((second-elements-value-hash-table
            (make-hash-table
              element-value-equal?
              element-value-hash)))
      (map
        (lambda (second-element-value)
          (hash-table-set!
            second-elements-value-hash-table
            second-element-value
            #t))
        second-elements-value)

      ;; sort the returned indexes
      (sort

        ;; search the first elements value
        ;; among the second elements value
        (filter-map
          (lambda (first-element-value-with-index)
            (let ((first-element-value (car first-element-value-with-index))
                  (first-element-index (cadr first-element-value-with-index)))
              (if (hash-table-ref/default second-elements-value-hash-table first-element-value #f)
                (if keep-matches-index first-element-index #f)
                (if keep-non-matches-index first-element-index #f))))

          ;; zip the first elements value
          ;; with their index
          (zip first-elements-value (iota (length first-elements-value))))

        <))))

;; returns the index at which the elements
;; of two lists share the same value or not
(define (list-same-or-different-values-index-intern
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure
          keep-same-values-index
          keep-different-values-index)

  ;; compare the first elements value
  ;; to the second elements value
  (filter-map
    (lambda (first-and-second-element-value-with-index)
      (let* ((first-element-value (car first-and-second-element-value-with-index))
             (second-element-value (cadr first-and-second-element-value-with-index))
             (index (caddr first-and-second-element-value-with-index)))
        (if (equal? first-element-value second-element-value)
          (if keep-same-values-index index #f)
          (if keep-different-values-index index #f))))

    ;; zip the first elements value,
    ;; the second elements value,
    ;; and their index
    (zip
      (map first-element-value-procedure first-elements)
      (map second-element-value-procedure second-elements)
      (iota (length first-elements)))))

;; returns the element having
;; the minimum or maximum value
(define (list-minimum-or-maximum-element-intern
          elements
          element-value-procedure
          comparaison-procedure)
  (if (not (null? elements))
    (fold 
      (lambda (element minimum-or-maximum-element)
        (if (comparaison-procedure
              (element-value-procedure element)
              (element-value-procedure minimum-or-maximum-element))
          element
          minimum-or-maximum-element))
      (car elements)
      (cdr elements))
    #f))
