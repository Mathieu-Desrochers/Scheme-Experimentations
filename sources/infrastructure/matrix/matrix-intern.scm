
(use srfi-1)

(declare (unit matrix-intern))

(declare (uses exceptions))
(declare (uses hungarian))

;; convert matrix coordinates to a vector index
(define (matrix-vector-index matrix row-index column-index)
  (+ (* row-index (matrix-columns-count matrix)) column-index))

;; sets a matrix row
(define (matrix-row-set!-intern matrix values row-index column-index)
  (when (< column-index (matrix-columns-count matrix))

    (matrix-value-set!
      matrix
      row-index
      column-index
      (car values))

    (matrix-row-set!-intern
      matrix
      (cdr values)
      row-index
      (+ column-index 1))))

;; sets a matrix column
(define (matrix-column-set!-intern matrix values row-index column-index)
  (when (< row-index (matrix-rows-count matrix))

    (matrix-value-set!
      matrix
      row-index
      column-index
      (car values))

    (matrix-column-set!-intern
      matrix
      (cdr values)
      (+ row-index 1)
      column-index)))

;; invokes a procedure with a hungarian-problem*
(define (with-hungarian-problem* procedure)
  (define (checked-malloc-hungarian-problem)
    (let ((hungarian-problem* (malloc-hungarian-problem)))
      (if (not hungarian-problem*)
        (abort "failed to allocate hungarian-problem"))
        hungarian-problem*))
  (with-guaranteed-release
    checked-malloc-hungarian-problem
    procedure
    free-hungarian-problem))

;; invokes a procedure with a hungarian-cost-matrix***
(define (with-hungarian-cost-matrix*** rows-count columns-count procedure)
  (define (checked-malloc-hungarian-cost-matrix**)
    (let ((hungarian-cost-matrix*** (malloc-hungarian-cost-matrix** rows-count columns-count)))
      (if (not hungarian-cost-matrix***)
        (abort "failed to allocate hungarian-cost-matrix"))
        hungarian-cost-matrix***))
  (with-guaranteed-release
    checked-malloc-hungarian-cost-matrix**
    procedure
    free-hungarian-cost-matrix**))

;; solves the assignment problem represented by a matrix
(define (matrix-solve-assignment-problem-intern matrix maximize)

  ;; precalculate these values
  (let ((matrix-rows-count (matrix-rows-count matrix))
        (matrix-columns-count (matrix-columns-count matrix)))

    ;; allocate the hungarian cost matrix
    (with-hungarian-cost-matrix***
      matrix-rows-count
      matrix-columns-count
      (lambda (hungarian-cost-matrix***)

      ;; initialize the hungarian cost matrix
      (matrix-for-each
        matrix
        (lambda (row-index column-index)
          (let ((matrix-value (matrix-value matrix row-index column-index)))
            (hungarian-cost-matrix-value-set!
              hungarian-cost-matrix***
              row-index
              column-index
              matrix-value))))

      ;; allocate the hungarian problem
      (with-hungarian-problem*
        (lambda (hungarian-problem*)

          ;; initialize the hungarian problem
          (hungarian-init
            hungarian-problem*
            (indirect-hungarian-cost-matrix*** hungarian-cost-matrix***)
            matrix-rows-count
            matrix-columns-count
            (if maximize
              hungarian-mode-maximize-util
              hungarian-mode-minimize-cost))

          ;; solve the hungarian problem
          (hungarian-solve hungarian-problem*)

          ;; build the assignments
          (fold
            (lambda (index accumulator)
              (let ((row-index (quotient index matrix-columns-count))
                    (column-index (modulo index matrix-columns-count)))
                (if (eq? (hungarian-problem-assignment-matrix-value hungarian-problem* row-index column-index) hungarian-assigned)
                  (append accumulator (list (cons row-index column-index)))
                  accumulator)))
            (list)
            (iota (* matrix-rows-count matrix-columns-count)))))))))
