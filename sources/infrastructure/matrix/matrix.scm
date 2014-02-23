
(use srfi-1)

(declare (unit matrix))

(declare (uses matrix-intern))

;; makes a matrix
(define (make-matrix rows-count columns-count)
  (list-tabulate
    rows-count
    (lambda (row-index)
      (make-list columns-count #f))))

;; returns the number of rows of a matrix
(define (matrix-rows-count matrix)
  (length matrix))

;; returns the number of columns of a matrix
(define (matrix-columns-count matrix)
  (if (eq? (length matrix) 0)
    0
    (length (car matrix))))

;; gets a matrix value
(define (matrix-value matrix row-index column-index)
  (list-ref
    (list-ref
      matrix
      row-index)
    column-index))

;; sets a matrix value
(define (matrix-value-set! matrix row-index column-index value)
  (set-car!
    (drop
      (list-ref
        matrix
        row-index)
      column-index)
    value))

;; returns a matrix row
(define (matrix-row matrix row-index)
  (list-ref
    matrix
    row-index))

;; returns a matrix column
(define (matrix-column matrix column-index)
  (map
    (lambda (row)
      (list-ref
        row
        column-index))
    matrix))

;; sets a matrix row
(define (matrix-row-set! matrix values row-index)
  (set-car!
    (drop
      matrix
      row-index)
    values)
  matrix)

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
