
(use data-structures)
(use srfi-69)

(declare (unit compare))

;; encapsulates a compare result
(define-record compare-result element status)

;; compares two sets of elements
;; elements are matched according to their id
(define
  (compare-by-id
    original-elements
    original-element-id-procedure
    current-elements
    current-element-id-procedure
    element-changed?-procedure
    make-added-compare-result-element-procedure
    make-changed-compare-result-element-procedure
    make-unchanged-compare-result-element-procedure
    make-deleted-compare-result-element-procedure)

  ;; make the hash-tables
  (let ((original-elements-hashtable (make-hash-table = number-hash))
        (current-elements-hashtable (make-hash-table = number-hash)))

    ;; hashes elements according to their id
    ;; provides unique negative ids for elements without one
    (define (hash-elements elements hashtable element-id-procedure)
      (map
        (lambda (element-index)
          (let* ((element (list-ref elements element-index))
                 (element-id (element-id-procedure element)))
            (if element-id
              (hash-table-set! hashtable element-id element)
              (hash-table-set! hashtable (+ -10000000 element-index) element))))
        (iota (length elements))))

    ;; hash the elements according to their id
    (hash-elements original-elements original-elements-hashtable original-element-id-procedure)
    (hash-elements current-elements current-elements-hashtable current-element-id-procedure)

    ;; merge the elements id
    (let* ((original-elements-id (hash-table-keys original-elements-hashtable))
           (current-elements-id (hash-table-keys current-elements-hashtable))
           (elements-id (sort (lset-union eq? original-elements-id current-elements-id) <)))

      ;; makes an added compare result
      (define (make-added-compare-result current-element)
        (make-compare-result (make-added-compare-result-element-procedure current-element) 'added))

      ;; makes an changed/unchanged compare result
      (define (make-changed/unchanged-compare-result original-element current-element)
        (if (element-changed?-procedure original-element current-element)
          (make-compare-result (make-changed-compare-result-element-procedure original-element current-element) 'changed)
          (make-compare-result (make-unchanged-compare-result-element-procedure original-element current-element) 'unchanged)))
        
      ;; makes a deleted compare result
      (define (make-deleted-compare-result original-element)
        (make-compare-result (make-deleted-compare-result-element-procedure original-element) 'deleted))

      ;; compares an element
      (define (compare-element element-id)
        (let ((original-element (hash-table-ref/default original-elements-hashtable element-id #f))
              (current-element (hash-table-ref/default current-elements-hashtable element-id #f)))
          (cond ((not original-element) (make-added-compare-result current-element))
                ((not current-element) (make-deleted-compare-result original-element))
                (else (make-changed/unchanged-compare-result original-element current-element)))))

      ;; compare the elements
      (map
        compare-element
        elements-id))))
