
list-duplicates-index
---------------------
Returns the index of the elements whose value is duplicated.

__elements__  
The list of elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 2822 "Ninja costume" 3)
      (make-order-item-row 1005 "Chocolate bar" 2))

__element-value-procedure__  
A procedure that returns the value of an element.

    order-item-row-id

__result__  
The index of the duplicates.

    '(0 2)

list-matches-index
------------------
Returns the index of the left elements whose value  
is found among the values of the right elements.

__left-elements__  
The list of left elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 2822 "Ninja costume" 3)
      (make-order-item-row 6027 "Soccer ball" 1))

__left-element-value-procedure__  
A procedure that returns the value of a left element.

    order-item-row-id

__right-elements__  
The list of right elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 6027 "Baseball hat" 2))

__right-element-value-procedure__  
A procedure that returns the value of a right element.

    order-item-row-id

__result__  
The index of the matching left elements. 

    '(0 2)

list-non-matches-index
----------------------
Returns the index of the left elements whose value  
is not found among the values of the right elements.

__left-elements__  
The list of left elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 2822 "Ninja costume" 3)
      (make-order-item-row 6027 "Soccer ball" 1))

__left-element-value-procedure__  
A procedure that returns the value of a left element.

    order-item-row-id

__right-elements__  
The list of right elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 6027 "Baseball hat" 2))

__right-element-value-procedure__  
A procedure that returns the value of a right element.

    order-item-row-id

__result__  
The index of the non matching left elements. 

    '(1)

list-sort
---------
Sorts a list of elements according to their sort value.

__elements__  
The list of elements.

    (list
      (make-order-item-row 1005 "Chocolate bar" 5)
      (make-order-item-row 2822 "Ninja costume" 3)
      (make-order-item-row 6027 "Soccer ball" 1))

__element-sort-value-procedure__  
A procedure that returns the sort value of an element.

    order-item-row-quantity

__result__  
The list of sorted elements.

    (list
      (make-order-item-row 6027 "Soccer ball" 1)
      (make-order-item-row 2822 "Ninja costume" 3)
      (make-order-item-row 1005 "Chocolate bar" 5))

list-is-consecutive-sequence
----------------------------
Returns whether a list of elements has values  
that make a consecutive sequence.

__elements__  
The list of elements.

    (list
      (make-pirate-level 1 "Caring")
      (make-pirate-level 3 "Evil")
      (make-pirate-level 2 "Brutish"))

__element-value-procedure__
A procedure that returns the value of an element.

    pirate-level-rank

__result__
Whether the values make a consecutive sequence.

    #t
