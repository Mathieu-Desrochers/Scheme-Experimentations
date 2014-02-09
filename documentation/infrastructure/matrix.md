
make-matrix
-----------
Makes a matrix.

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

    #f

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
Sets a matrix row values.

__matrix__  
The matrix.

    (make-matrix 2 3)

__values__  
The row values.

    (list 1 2 3)

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
