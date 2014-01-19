
(use data-structures)
(use srfi-1)
(use srfi-69)

(declare (unit list))

(declare (uses date-time))
(declare (uses list-intern))

;; returns the index of the elements
;; that appear more than once in a list
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
(define (list-number-duplicates-index
          elements
          element-value-procedure)

  (list-duplicates-index-intern
    elements
    element-value-procedure
    =
    number-hash))

;; returns the index of the elements in a first list
;; whose value can be matched in a second list
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

;; returns the element in a list that has the maximim value
(define (list-maximum-element
          elements
          element-value-procedure)

  (list-minimum-or-maximum-element-intern
    elements
    element-value-procedure
    >))

;; returns a list of all the possible element combinaisons
(define (list-combinaisons elements)
  (map
    (lambda (bit-mask)
      (fold

        ;; returns the elements
        ;; flagged in the bit-mask
        (lambda (bit-index flagged-elements)
          (if (bit-set? bit-mask bit-index)
            (append
              flagged-elements
              (list (list-ref elements bit-index)))
            flagged-elements))
        '()

        ;; check every bit in the bit-mask
        ;; 000, 000, 000...
        ;;   ^   ^   ^
        (iota (length elements))))

    ;; build the bit-masks representing
    ;; all the element index combinaisons
    ;; 000, 001, 010, 011...
    (iota (expt 2 (length elements)))))

;; returns the sum of the elements value
(define (list-sum
          elements
          element-value-procedure)
  (fold + 0 (map element-value-procedure elements)))

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

  ;; inner procedure that accumulates the result
  (define (list-filtered-index-inner elements-inner index result)
    (if (null? elements-inner)
      result
      (if (filter-procedure (element-value-procedure (car elements-inner)))
        (list-filtered-index-inner
          (cdr elements-inner)
          (+ index 1)
          (cons index result))
        (list-filtered-index-inner
          (cdr elements-inner)
          (+ index 1)
          result))))

  ;; reverse the result
  (reverse
    (list-filtered-index-inner
      elements
      0
      (list))))
