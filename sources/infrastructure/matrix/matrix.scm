
(use srfi-1)
(use srfi-4)

(declare (unit matrix))

(declare (uses matrix-intern))

;; encapsulates a matrix
(define-record matrix rows-count columns-count vector)

;; makes a new matrix
(define (new-matrix rows-count columns-count)
  (make-matrix
    rows-count
    columns-count
    (make-s32vector
      (* rows-count columns-count)
      0)))

;; gets a matrix value
(define (matrix-value matrix row-index column-index)
  (s32vector-ref
    (matrix-vector matrix)
    (matrix-vector-index matrix row-index column-index)))

;; sets a matrix value
(define (matrix-value-set! matrix row-index column-index value)
  (s32vector-set!
    (matrix-vector matrix)
    (matrix-vector-index matrix row-index column-index)
    value))

;; returns a matrix row
(define (matrix-row matrix row-index)
  (map
    (lambda (column-index)
      (matrix-value
        matrix
        row-index
        column-index))
    (iota (matrix-columns-count matrix))))

;; returns a matrix column
(define (matrix-column matrix column-index)
  (map
    (lambda (row-index)
      (matrix-value
        matrix
        row-index
        column-index))
    (iota (matrix-rows-count matrix))))

;; sets a matrix row
(define (matrix-row-set! matrix values row-index)
  (matrix-row-set!-intern
    matrix
    values
    row-index
    0))

;; sets a matrix column
(define (matrix-column-set! matrix values column-index)
  (matrix-column-set!-intern
    matrix
    values
    0
    column-index))

;; invokes a procedure for all the matrix elements
(define (matrix-for-each matrix procedure)
  (for-each
    (lambda (row-index)
      (for-each
        (lambda (column-index)
          (procedure row-index column-index))
        (iota (matrix-columns-count matrix))))
    (iota (matrix-rows-count matrix))))

;; solves the minimum assignment problem represented by a matrix
(define (matrix-solve-minimum-assignment-problem matrix)
  (matrix-solve-assignment-problem-intern matrix #f))

;; solves the maximum assignment problem represented by a matrix
(define (matrix-solve-maximum-assignment-problem matrix)
  (matrix-solve-assignment-problem-intern matrix #t))
