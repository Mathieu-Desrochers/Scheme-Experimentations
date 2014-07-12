
(use data-structures)
(use srfi-1)
(use srfi-69)

(declare (unit list))

(declare (uses date-time))
(declare (uses hash))
(declare (uses list-intern))

;; returns the index of the elements
;; that appear more than once in a list
;; ignores the false element values
(define (list-duplicates-index
          elements
          element-value-procedure)

  (list-duplicates-index-intern
    elements
    element-value-procedure
    equal?
    equal?-hash))

;; returns the index of the numeric elements
;; that appear more than once in a list
;; ignores the false element values
(define (list-number-duplicates-index
          elements
          element-value-procedure)

  (list-duplicates-index-intern
    elements
    element-value-procedure
    =
    number-hash))

;; returns the index of the string elements
;; that appear more than once in a list
;; ignores the false element values
(define (list-string-duplicates-index
          elements
          element-value-procedure)

  (list-duplicates-index-intern
    elements
    element-value-procedure
    string=?
    string-hash))

;; returns the index of the elements in a first list
;; whose value can be matched in a second list
;; ignores the false element values
(define (list-matches-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure)

  (list-matches-or-non-matches-index-intern
    first-elements
    first-element-value-procedure
    second-elements
    second-element-value-procedure
    equal?
    equal?-hash
    #t
    #f))

;; returns the index of the elements in a first list
;; whose value cannot be matched in a second list
;; ignores the false element values
(define (list-non-matches-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure)

  (list-matches-or-non-matches-index-intern
    first-elements
    first-element-value-procedure
    second-elements
    second-element-value-procedure
    equal?
    equal?-hash
    #f
    #t))

;; returns the index of the elements in a first list
;; whose numeric value can be matched in a second list
;; ignores the false element values
(define (list-number-matches-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure)

  (list-matches-or-non-matches-index-intern
    first-elements
    first-element-value-procedure
    second-elements
    second-element-value-procedure
    =
    number-hash
    #t
    #f))

;; returns the index of the elements in a first list
;; whose numeric value cannot be matched in a second list
;; ignores the false element values
(define (list-number-non-matches-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure)

  (list-matches-or-non-matches-index-intern
    first-elements
    first-element-value-procedure
    second-elements
    second-element-value-procedure
    =
    number-hash
    #f
    #t))

;; returns the index of the elements in a first list
;; whose string value can be matched in a second list
;; ignores the false element values
(define (list-string-matches-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure)

  (list-matches-or-non-matches-index-intern
    first-elements
    first-element-value-procedure
    second-elements
    second-element-value-procedure
    string=?
    string-hash
    #t
    #f))

;; returns the index of the elements in a first list
;; whose string value cannot be matched in a second list
;; ignores the false element values
(define (list-string-non-matches-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure)

  (list-matches-or-non-matches-index-intern
    first-elements
    first-element-value-procedure
    second-elements
    second-element-value-procedure
    string=?
    string-hash
    #f
    #t))

;; returns the index at which the elements
;; of two lists share the same value
(define (list-same-values-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure)

  (list-same-or-different-values-index-intern
    first-elements
    first-element-value-procedure
    second-elements
    second-element-value-procedure
    #t
    #f))

;; returns the index at which the elements
;; of two lists do not share the same value
(define (list-different-values-index
          first-elements
          first-element-value-procedure
          second-elements
          second-element-value-procedure)

  (list-same-or-different-values-index-intern
    first-elements
    first-element-value-procedure
    second-elements
    second-element-value-procedure
    #f
    #t))

;; sorts a list of elements by their numeric value
(define (list-sort-by-number
          elements
          element-sort-value-procedure)
  (sort
    elements
    (lambda (x y)
      (< (element-sort-value-procedure x)
         (element-sort-value-procedure y)))))

;; sorts a list of elements by their numeric value
(define (list-sort-by-number-desc
          elements
          element-sort-value-procedure)
  (sort
    elements
    (lambda (x y)
      (> (element-sort-value-procedure x)
         (element-sort-value-procedure y)))))

;; sorts a list of elements by their string value
(define (list-sort-by-string
          elements
          element-sort-value-procedure)
  (sort
    elements
    (lambda (x y)
      (<
        (string-compare3
          (element-sort-value-procedure x)
          (element-sort-value-procedure y))
        0))))

;; sorts a list of elements by their date value
(define (list-sort-by-date
          elements
          element-sort-value-procedure)

  ;; returns a sortable string for the
  ;; date value of an element
  (define (sortable-string element)
    (let ((date (element-sort-value-procedure element)))
      (date->string date)))

  ;; sort the elements according to their sortable strings
  (list-sort-by-string
    elements
    sortable-string))

;; sorts a list of elements by their day-of-week and time values
(define (list-sort-by-day-of-week-and-time
          elements
          element-day-of-week-sort-value-procedure
          element-time-sort-value-procedure)

  ;; returns a sortable string for the
  ;; day-of-week and time values of an element
  (define (sortable-string element)
    (let ((day-of-week (element-day-of-week-sort-value-procedure element))
          (time (element-time-sort-value-procedure element)))
      (string-append
        (number->string (day-of-week->integer day-of-week))
        (time->string time))))

  ;; sort the elements according to their sortable strings
  (list-sort-by-string
    elements
    sortable-string))

;; returns whether the elements in a list form a
;; consecutive sequence that starts from one
(define (list-is-consecutive-sequence
          elements
          element-value-procedure)
  (equal?
    (sort (map element-value-procedure elements) <)
    (iota (length elements) 1)))

;; returns the element in a list that has the minimum value
(define (list-minimum-element
          elements
          element-value-procedure)

  (list-minimum-or-maximum-element-intern
    elements
    element-value-procedure
    <))

;; returns the element in a list that has the maximum value
(define (list-maximum-element
          elements
          element-value-procedure)

  (list-minimum-or-maximum-element-intern
    elements
    element-value-procedure
    >))

;; returns the number of elements
;; whose value matches a filter
(define (list-filtered-count
          elements
          element-value-procedure
          filter-procedure)
  (length
    (filter
      filter-procedure
      (map
        element-value-procedure
        elements))))

;; returns the index of the elements
;; whose value matches a filter
(define (list-filtered-index
          elements
          element-value-procedure
          filter-procedure)

  (reverse
    (list-filtered-index-inner
      elements
      0
      (list))))

;; returns the last index of a value in a list
(define (list-value-last-index
          elements
          element-value-procedure
          value)

  (list-value-last-index-intern
    elements
    element-value-procedure
    value
    0
    #f))

;; keeps only the lowest numbers in a list
(define (list-keep-lowest-numbers
          elements
          element-value-procedure
          count)

  ;; check if no elements must be kept
  (if (eq? count 0)
    (map
      (lambda (element) #f)
      elements)

    ;; check if all the elements must be kept
    (if (>= count (length elements))
      elements

      ;; sort the elements
      (let ((sorted-elements (list-sort-by-number elements element-value-procedure)))

        ;; get the highest allowed element value
        (let ((maximum-element-value
                (if (> (length sorted-elements) count)
                  (list-ref sorted-elements (- count 1))
                  (last sorted-elements))))

          ;; get how many of the highest allowed element values
          ;; can be kept among all the ones found
          (let ((maximum-element-values-count
                  (length
                    (filter
                      (lambda (element-value)
                        (eq? element-value maximum-element-value))
                      (take sorted-elements count)))))

            ;; filter the elements
            (list-keep-lowest-numbers-intern
              elements
              element-value-procedure
              count
              maximum-element-value
              maximum-element-values-count
              (list))))))))

;; removes the elements at the given indexes
(define (list-remove-at-indexes
          elements
          indexes)

  ;; hash the indexes
  (let ((indexes-hash-table
          (hash-with-unique-numeric-keys
            indexes
            identity
            identity)))

    ;; remove the elements
    (list-remove-at-indexes-intern
      elements
      indexes-hash-table
      0
      (list))))

;; returns the index of elements that can be paired
;; with another element having a matching key
(define (list-matching-pairs-index
          elements
          element-key-procedure
          element-matching-key-procedure)

  ;; the hash key represents the matching key
  ;; the hash value is the index of the element
  (let ((matching-keys-hash-table
          (make-hash-table
            equal?
            equal?-hash)))

    ;; get the matching pairs index
    (list-matching-pairs-index-intern
      elements
      element-key-procedure
      element-matching-key-procedure
      matching-keys-hash-table
      0
      (list))))

;; removes the elements that can be paired
;; with another element having a matching key
(define (list-remove-matching-pairs
          elements
          element-key-procedure
          element-matching-key-procedure)

  ;; get the indexes of the matching pairs
  (let ((indexes
          (list-matching-pairs-index
            elements
            element-key-procedure
            element-matching-key-procedure)))

    (list-remove-at-indexes
      elements
      indexes)))

;; returns the index of the elements
;; having a given value
(define (list-value-indexes
          elements
          element-value-procedure
          value)

  (list-value-indexes-intern
    elements
    element-value-procedure
    value
    0
    (list)))

;; removes the elements having a given value
(define (list-remove-value
          elements
          element-value-procedure
          value)

  ;; get the indexes of the value
  (let ((indexes
          (list-value-indexes
            elements
            element-value-procedure
            value)))

    (list-remove-at-indexes
      elements
      indexes)))

;; a variant of the map procedure that returns only true values
;; guarantees to apply the procedure in the left-to-right order of the elements
(define (list-filter-map-in-order
          elements
          procedure)

  (filter
    identity
    (map-in-order
      procedure
      elements)))

;; returns the distinct element values
;; ignores the false element values
(define (list-distinct-values
          elements
          element-value-procedure)

  (list-distinct-values-intern
    elements
    element-value-procedure
    equal?
    equal?-hash))

;; returns the distinct numeric element values
;; ignores the false element values
(define (list-number-distinct-values
          elements
          element-value-procedure)

  (list-distinct-values-intern
    elements
    element-value-procedure
    =
    number-hash))
