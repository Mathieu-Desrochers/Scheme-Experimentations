
new-matrix
----------
Makes a new matrix.

__rows-count__  
The number of rows.

    2

__columns-count__  
The number of columns.

    3

__result__  
The matrix.

matrix-rows-count
-----------------
Returns the number of rows in a matrix.

__matrix__  
The matrix.

    (make-matrix 2 3)

__result__  
The number of rows.

    2

matrix-columns-count
--------------------
Returns the number of columns in a matrix.

__matrix__  
The matrix.

    (make-matrix 2 3)

__result__  
The number of columns.

    3

matrix-value
------------
Returns a matrix value.

__matrix__  
The matrix.

    (make-matrix 2 3)

__row-index__  
The row index.

    0

__column-index__  
The column index.

    2

__result__  
The value.

    0

matrix-value-set!
-----------------
Sets a matrix value.

__matrix__  
The matrix.

    (make-matrix 2 3)

__row-index__  
The row index.

    0

__column-index__  
The column index.

    2

__value__  
The value.

    25

matrix-row
----------
Returns a matrix row.

__matrix__  
The matrix.

    (make-matrix 2 3)

__row-index__  
The row index.

    0

__result__  
The row.

    (list #f #f #f)

matrix-column
-------------
Returns a matrix column.

__matrix__  
The matrix.

    (make-matrix 2 3)

__column-index__  
The column index.

    0

__result__  
The column.

    (list #f #f)

matrix-row-set!
---------------
Sets a matrix row.

__matrix__  
The matrix.

    (make-matrix 2 3)

__values__  
The row values.

    (list 1 2 3)

__row-index__  
The row index.

    0

matrix-column-set!
------------------
Sets a matrix column.

__matrix__  
The matrix.

    (make-matrix 2 3)

__values__  
The column values.

    (list 1 2)

__column-index__  
The column index.

    0

matrix-for-each
---------------
Invokes a procedure for all the elements of a matrix.

__matrix__  
The matrix.

    (make-matrix 2 3)

__procedure__  
The procedure to invoke.

    (lambda (row-index column-index)
      (print row-index " " column-index))

__output__  
Produces the following output.

    0 0
    0 1
    0 2
    1 0
    1 1
    1 2

matrix-solve-minimum-assignment-problem
---------------------------------------
Solves the minimum assignment problem represented by a matrix.

__matrix__  
The matrix.

    (make-matrix 2 3)

    (matrix-value-set! matrix 0 0 1)
    (matrix-value-set! matrix 0 1 8)
    (matrix-value-set! matrix 0 2 5)
    (matrix-value-set! matrix 1 0 2)
    (matrix-value-set! matrix 1 1 1)
    (matrix-value-set! matrix 1 2 9)

__result__  
The pairs representing the assigned rows and columns.

    (list
      (0 . 0)
      (1 . 1))

matrix-solve-maximum-assignment-problem
---------------------------------------
Solves the maximum assignment problem represented by a matrix.

__matrix__  
The matrix.

    (make-matrix 2 3)

    (matrix-value-set! matrix 0 0 1)
    (matrix-value-set! matrix 0 1 8)
    (matrix-value-set! matrix 0 2 5)
    (matrix-value-set! matrix 1 0 2)
    (matrix-value-set! matrix 1 1 1)
    (matrix-value-set! matrix 1 2 9)

__result__  
The pairs representing the assigned rows and columns.

    (list
      (0 . 1)
      (1 . 2))
