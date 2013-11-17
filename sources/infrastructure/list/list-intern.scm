
(use data-structures)
(use srfi-1)
(use srfi-69)

(declare (unit list-intern))

;; returns the index of the elements in a first list
;; whose value can be matched in a second list or not
(define (list-matches-or-non-matches-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure
          keep-matches-index
          keep-non-matches-index)

  ;; get the first and second elements value
  (let ((first-elements-value (map first-element-value-procedure first-elements))
        (second-elements-value (map second-element-value-procedure second-elements)))

    ;; hash the second elements value
    (let ((second-elements-value-hash-table (make-hash-table = number-hash)))
      (map
        (lambda (second-element-value)
          (when second-element-value
            (hash-table-set!
              second-elements-value-hash-table
              second-element-value
              #t)))
        second-elements-value)

      ;; sort the returned indexes
      (sort

        ;; search the first elements value
        ;; among the second elements value
        (filter-map
          (lambda (first-element-value-with-index)
            (let ((first-element-value (car first-element-value-with-index))
                  (first-element-index (cadr first-element-value-with-index)))
              (if first-element-value
                (if (hash-table-ref/default second-elements-value-hash-table first-element-value #f)
                  (if keep-matches-index first-element-index #f)
                  (if keep-non-matches-index first-element-index #f))
                #f)))

          ;; zip the first elements value
          ;; with their index
          (zip first-elements-value (iota (length first-elements-value))))

        <))))

;; returns the index at which the elements
;; of two lists share the same value or not
(define (list-same-or-different-values-index
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
        (if (eq? first-element-value second-element-value)
          (if keep-same-values-index index #f)
          (if keep-different-values-index index #f))))

    ;; zip the first elements value,
    ;; the second elements value,
    ;; and their index
    (zip
      (map first-element-value-procedure first-elements)
      (map second-element-value-procedure second-elements)
      (iota (length first-elements)))))
