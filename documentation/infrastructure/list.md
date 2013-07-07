
list-duplicates-index
---------------------
Returns the index of the duplicates in a list.

__elements__  
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

__tested-elements__  
The first list of elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 2822 "Ninja costume" 3)
      (make-order-item-row 6001 "Soccer ball" 1))

__tested-element-value-procedure__  
A procedure that returns a tested element's value.

    (lambda (order-item-row)
      (order-item-row-id order-item-row))

__reference-elements__  
The second list of elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 6027 "Baseball hat" 2))

__reference-element-value-procedure__  
A procedure that returns a reference element's value.

    (lambda (order-item-row)
      (order-item-row-id order-item-row))

__result__  
Returns the index of the tested elements having a value  
that was not found among the reference elements.

    '(1 2)

list-sort
---------
Sorts a list of elements.

__elements__  
The list of elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 2822 "Ninja costume" 3)
      (make-order-item-row 6001 "Soccer ball" 1))

__element-sort-value-procedure__  
A procedure that returns an element's sort value.

    order-item-row-quantity

__element-value-procedure__  
A procedure that returns an element's value.

    order-item-row-product-name

__result__  
Returns a list of the elements' value, as they were ordered  
after being sorted by their sort value.

    (list
      "Soccer ball"
      "Ninja costume"
      "Chocolate bar")
