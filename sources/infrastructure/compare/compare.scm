
(use data-structures)
(use srfi-1)
(use srfi-69)

(declare (unit compare))

;; encapsulates compare results
(define-record compare-results added-elements changed-elements unchanged-elements deleted-elements)

;; compares two sets of elements
;; elements are matched according to their id
(define
  (compare-elements
    original-elements
    original-element-id-procedure
    current-elements
    current-element-id-procedure
    element-changed?-procedure
    make-added-compare-result-element-procedure
    make-changed-compare-result-element-procedure
    make-unchanged-compare-result-element-procedure
    make-deleted-compare-result-element-procedure)

  ;; make a hash-table for each set of elements
  (let ((original-elements-hashtable (make-hash-table = number-hash))
        (current-elements-hashtable (make-hash-table = number-hash)))

    ;; hashes elements according to their id
    ;; if the element-id-procedure returns false,
    ;; the next-negative-id is used instead
    (define (hash-elements elements hashtable element-id-procedure next-negative-id)
      (if (not (null? elements))
        (let* ((element (car elements))
               (element-id (element-id-procedure element)))
          (if element-id
            (hash-table-set! hashtable element-id element)
            (hash-table-set! hashtable next-negative-id element))
          (hash-elements (cdr elements) hashtable element-id-procedure (- next-negative-id 1)))))

    ;; hash the elements according to their id
    (hash-elements original-elements original-elements-hashtable original-element-id-procedure -1)
    (hash-elements current-elements current-elements-hashtable current-element-id-procedure
      (- 0 (length original-elements) 1))

    ;; combine the elements id
    (let* ((original-elements-id (hash-table-keys original-elements-hashtable))
           (current-elements-id (hash-table-keys current-elements-hashtable))
           (elements-id (sort (lset-union eq? original-elements-id current-elements-id) <)))

      ;; makes an added compare result
      (define (make-added-compare-result current-element)
        (cons (make-added-compare-result-element-procedure current-element) 'added))

      ;; makes an changed/unchanged compare result
      (define (make-changed/unchanged-compare-result original-element current-element)
        (if (element-changed?-procedure original-element current-element)
          (cons (make-changed-compare-result-element-procedure original-element current-element) 'changed)
          (cons (make-unchanged-compare-result-element-procedure original-element current-element) 'unchanged)))
        
      ;; makes a deleted compare result
      (define (make-deleted-compare-result original-element)
        (cons (make-deleted-compare-result-element-procedure original-element) 'deleted))

      ;; compares an element
      (define (compare-element element-id)
        (let ((original-element (hash-table-ref/default original-elements-hashtable element-id #f))
              (current-element (hash-table-ref/default current-elements-hashtable element-id #f)))
          (cond ((not original-element) (make-added-compare-result current-element))
                ((not current-element) (make-deleted-compare-result original-element))
                (else (make-changed/unchanged-compare-result original-element current-element)))))

      ;; compare the elements
      (let ((compare-results (map compare-element elements-id)))

        ;; filters the compare results element
        (define (filter-compare-results-element status)
          (map car
            (filter
              (lambda (compare-result)
                (eq? (cdr compare-result) status))
              compare-results)))

        ;; make the compare results
        (make-compare-results
          (filter-compare-results-element 'added)
          (filter-compare-results-element 'changed)
          (filter-compare-results-element 'unchanged)
          (filter-compare-results-element 'deleted))))))
