
compare-elements
----------------
Compares two set of elements.

__original-elements__  

    (list
      ('order-item-row 1005 "Chocolate bar" 5)
      ('order-item-row 2822 "Ninja costume" 3)
      ('order-item-row 6001 "Soccer ball" 1))

__original-element-id-procedure__  
Procedure that returns an original element's id.  
This would be the four digit numbers in this example.

__current-elements__

    (list
      ('order-item-subrequest 1005 "Chocolate bar" 25)
      ('order-item-subrequest 2822 "Ninja costume" 3)
      ('order-item-subrequest 6027 "Baseball hat" 2))

__current-element-id-procedure__  
Procedure that returns a current element's id.  
This would be the four digit numbers in this example.

__element-changed?-procedure__  
Procedure invoked with every pair of original and current elements that match by id.  
Must return whether the elements are considered to have changed.

This pair would have changed :  

    ('order-item-row 1005 "Chocolate bar" 5)
    ('order-item-subrequest 1005 "Chocolate bar" 25)

This pair would not :

    ('order-item-row 2822 "Ninja costume" 3)
    ('order-item-subrequest 2822 "Ninja costume" 3)

__make-added-compare-result-element-procedure__  
Procedure invoked with every current element that was not matched.

    ('order-item-subrequest 6027 "Baseball hat" 2)

Has to return an element representing the addition.

    ('start-item-production-request 6027 2)

__make-changed-compare-result-element-procedure__  
Procedure invoked with every pair of matched elements that have changed.

    ('order-item-row 1005 "Chocolate bar" 5)
    ('order-item-subrequest 1005 "Chocolate bar" 25)

Has to return an element representing the modification.

    ('alter-item-production-request 1005 +20)

__make-unchanged-compare-result-element-procedure__  
Procedure invoked with every pair of matched elements that have not changed.

    ('order-item-row 2822 "Ninja costume" 3)
    ('order-item-subrequest 2822 "Ninja costume" 3)

Has to return an element representing the non modification.  
This would be #f in this example.

__make-deleted-compare-result-element-procedure__  
Procedure invoked with every unmatched original elements.

    ('order-item-row 6001 "Soccer ball" 1)

Has to return an element representing the element deletion.  

    (stop-item-production-request 6001 1)

__result__  
Returns the four following lists.  
added-elements:

    (list ('start-item-production-request 6027 2))

changed-elements:

    (list ('alter-item-production-request 1005 +20))

unchanged-elements:

    (list #f)

deleted-elements:

    (list (stop-item-production-request 6001 1))
