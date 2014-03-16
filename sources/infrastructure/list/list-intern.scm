
(use data-structures)
(use srfi-1)
(use srfi-69)

(declare (unit list-intern))

;; returns the index of the elements
;; that appear more than once in a list
;; ignores the false element values
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

;; returns the index of the elements in a first list
;; whose value can be matched in a second list or not
;; ignores the false element values
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

;; returns the index of the elements
;; whose value matches a filter
(define (list-filtered-index-inner
          elements
          index
          result)
  (if (null? elements)
    result
    (if (filter-procedure (element-value-procedure (car elements)))
      (list-filtered-index-inner
        (cdr elements)
        (+ index 1)
        (cons index result))
      (list-filtered-index-inner
        (cdr elements)
        (+ index 1)
        result))))

;; returns the last index of a value in a list
(define (list-value-last-index-intern
          elements
          element-value-procedure
          value
          current-index
          value-last-index)
  (if (null? elements)
    value-last-index
    (let ((value-last-index
            (if (eq? (element-value-procedure (car elements)) value)
              current-index
              value-last-index)))
      (list-value-last-index-intern
        (cdr elements)
        element-value-procedure
        value
        (+ current-index 1)
        value-last-index))))

;; keeps only the lowest numbers in a list
(define (list-keep-lowest-numbers-intern
          elements
          element-value-procedure
          count
          maximum-element-value
          maximum-element-values-count
          accumulator)

  ;; check if all the elements have been filtered
  (if (null? elements)
    (reverse accumulator)

    ;; get the element value
    (let* ((element (car elements))
           (element-value (element-value-procedure element)))

      ;; check if we can still keep elements
      (if (eq? count 0)

        ;; discard the element value
        (list-keep-lowest-numbers-intern
          (cdr elements)
          element-value-procedure
          count
          maximum-element-value
          maximum-element-values-count
          (cons #f accumulator))

        ;; check if the element value
        ;; is below the maximum allowed
        (if (< element-value maximum-element-value)

          ;; keep the element value
          (list-keep-lowest-numbers-intern
            (cdr elements)
            element-value-procedure
            (- count 1)
            maximum-element-value
            maximum-element-values-count
            (cons element accumulator))

          ;; check if the element value
          ;; is equal to the maximum allowed
          (if (eq? element-value maximum-element-value)

            ;; check if we can still keep maximum elements
            (if (eq? maximum-element-values-count 0)

              ;; discard the maximum element value
              (list-keep-lowest-numbers-intern
                (cdr elements)
                element-value-procedure
                count
                maximum-element-value
                maximum-element-values-count
                (cons #f accumulator))

              ;; keep the maximum element value
              (list-keep-lowest-numbers-intern
                (cdr elements)
                element-value-procedure
                (- count 1)
                maximum-element-value
                (- maximum-element-values-count 1)
                (cons element accumulator)))

            ;; discard the element value
            (list-keep-lowest-numbers-intern
              (cdr elements)
              element-value-procedure
              count
              maximum-element-value
              maximum-element-values-count
              (cons #f accumulator))))))))

;; removes the elements at the given indexes
(define (list-remove-at-indexes-intern
          elements
          indexes-hash-table
          current-index
          accumulator)

  ;; check if all the elements have been filtered
  (if (null? elements)
    (reverse accumulator)

    ;; check if the current index must be removed
    (if (hash-table-exists? indexes-hash-table current-index)

      ;; remove the element
      (list-remove-at-indexes-intern
        (cdr elements)
        indexes-hash-table
        (+ current-index 1)
        accumulator)

      ;; keep the element
      (list-remove-at-indexes-intern
        (cdr elements)
        indexes-hash-table
        (+ current-index 1)
        (cons (car elements) accumulator)))))

;; returns the index of elements that can be paired
;; with another element having a matching key
(define (list-matching-pairs-index-intern
          elements
          element-key-procedure
          element-matching-key-procedure
          matching-keys-hash-table
          index
          matching-pairs-index)

  ;; check if all the elements have been paired
  (if (null? elements)
    (reverse matching-pairs-index)

    ;; get the element keys
    (let* ((element (car elements))
           (element-key (element-key-procedure element))
           (element-matching-key (element-matching-key-procedure element)))

      ;; check if a matching element was previously found
      (let ((matching-index
              (hash-table-ref/default
                matching-keys-hash-table
                element-matching-key
                #f)))

        ;; a match was found
        (if matching-index
          (begin

            ;; remove the matching key
            (hash-table-delete!
              matching-keys-hash-table
              element-matching-key)

            ;; keep track of the matching indexes
            (list-matching-pairs-index-intern
              (cdr elements)
              element-key-procedure
              element-matching-key-procedure
              matching-keys-hash-table
              (+ index 1)
              (cons index (cons matching-index matching-pairs-index))))

          ;; no match was found
          (begin

            ;; add the matching key
            (hash-table-set!
              matching-keys-hash-table
              element-key
              index)

            ;; keep looking for matching indexes
            (list-matching-pairs-index-intern
              (cdr elements)
              element-key-procedure
              element-matching-key-procedure
              matching-keys-hash-table
              (+ index 1)
              matching-pairs-index)))))))

;; returns the index of the elements
;; having a given value
(define (list-value-indexes-intern
          elements
          element-value-procedure
          value
          index
          accumulator)

  ;; check if all the elements have been filtered
  (if (null? elements)
    (reverse accumulator)

    ;; get the element value
    (let* ((element (car elements))
           (element-value (element-value-procedure element)))

      ;; check for the value
      (if (eq? element-value value)

        ;; keep the element index
        (list-value-indexes-intern
          (cdr elements)
          element-value-procedure
          value
          (+ index 1)
          (cons index accumulator))

        ;; ignore the element index
        (list-value-indexes-intern
          (cdr elements)
          element-value-procedure
          value
          (+ index 1)
          accumulator)))))
