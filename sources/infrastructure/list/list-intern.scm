
(use data-structures)
(use srfi-1)
(use srfi-69)

(declare (unit list-intern))

;; returns the index of the left elements
;; whose value is either matched or unmatched
;; among the right elements value
(define (list-match-elements-value-index
          left-elements
          left-element-value-procedure
          right-elements
          right-element-value-procedure
          keep-matches-index
          keep-non-matches-index)

  ;; get the left and right elements value
  (let ((left-elements-value (map left-element-value-procedure left-elements))
        (right-elements-value (map right-element-value-procedure right-elements)))

    ;; hash the right elements value
    (let ((right-elements-value-hash-table (make-hash-table = number-hash)))
      (map
        (lambda (right-element-value)
          (hash-table-set!
            right-elements-value-hash-table
            right-element-value
            #t))
        right-elements-value)

      ;; sort the left elements index
      (sort

        ;; search the left elements value
        ;; among the right ones
        (filter-map
          (lambda (left-element-value-with-index)
            (let ((left-element-value (car left-element-value-with-index))
                  (left-element-index (cadr left-element-value-with-index)))
              (if (hash-table-ref/default right-elements-value-hash-table left-element-value #f)
                (if keep-matches-index left-element-index #f)
                (if keep-non-matches-index left-element-index #f))))

          ;; pair the left elements value
          ;; with their index
          (zip left-elements-value (iota (length left-elements-value))))

        <))))
