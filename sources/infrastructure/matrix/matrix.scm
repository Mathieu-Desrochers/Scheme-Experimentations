
(use srfi-1)

(declare (unit matrix))

(declare (uses list))

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
  (length (car matrix)))

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

;; sets a matrix row values
(define (matrix-row-set! matrix values row-index)
  (set-car!
    (drop
      matrix
      row-index)
    values)
  matrix)

;; invokes a procedure for all the matrix elements
(define (matrix-for-each matrix procedure)
  (map
    (lambda (row-index)
      (map
        (lambda (column-index)
          (procedure row-index column-index))
        (iota (matrix-columns-count matrix))))
    (iota (matrix-rows-count matrix))))

;; keeps only one numeric element
;; by row and column, while optimizing
;; the overall sum of the matrix elements
(define (matrix-optimize-numbers matrix)
  1)