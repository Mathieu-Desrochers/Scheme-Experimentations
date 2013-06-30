
list-duplicates-index
---------------------
Returns the index of the duplicates in a list.

__list__  
The list of elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 2822 "Ninja costume" 3)
      (make-order-item-row 1005 "Chocolate bar" 2))

__element-value-procedure__  
A procedure that returns an element's value.

    (lambda (order-item-row)
      (order-item-row-id order-item-row))

__result__  
Returns the index of the elements having a duplicated value.

    '(0 2)

list-differences-index
----------------------
Returns the index of the elements in a first list  
that are missing in a second list.

__tested-list__  
The first list of elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 2822 "Ninja costume" 3)
      (make-order-item-row 6001 "Soccer ball" 1))

__tested-element-value-procedure__  
A procedure that returns a tested element's value.

    (lambda (order-item-row)
      (order-item-row-id order-item-row))

__reference-list__  
The second list of elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 6027 "Baseball hat" 2))

__reference-element-value-procedure__  
A procedure that returns a reference element's value.

    (lambda (order-item-row)
      (order-item-row-id order-item-row))

__returns__  
Returns the index of the tested elements having a value  
that was not found among the reference elements.

    '(1 2)
