
math-sum
--------
Returns the sum of the elements value.

__elements__  
The list of elements.

    (list 1 2 3)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__result__  
The sum of the elements value.

    6

math-average
------------
Returns the average of the elements value.

__elements__  
The list of elements.

    (list 1 2 3)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__result__  
The average of the elements value.

    2

math-average-add-value
----------------------
Adds a value to an previously computed average.  
Suppose it was computed from the values 10, 42 and 26.

__previous-average__  
The previously computed average.

	26

__previous-count__  
The number of values in the previously computed average.

	3

__value__  
The value added to the average.

    56

__result__  
The new average.

    33.5

math-average-remove-value
-------------------------
Removes a value from an previously computed average.  
Suppose it was computed from the values 10, 42, 26 and 56.

__previous-average__  
The previously computed average.

	33.5

__previous-count__  
The number of values in the previously computed average.

	4

__value__  
The value removed from the average.

    56

__result__  
The new average.

    26
