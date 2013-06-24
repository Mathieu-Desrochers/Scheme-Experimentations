
compare-elements
----------------
Compares two sets of elements.

__original-elements__  
The list of original elements.

    (list
      ('order-item-row 1005 "Chocolate bar" 5)
      ('order-item-row 2822 "Ninja costume" 3)
      ('order-item-row 6001 "Soccer ball" 1))

__original-element-id-procedure__  
A procedure that returns an original element's id.  

    (lambda (order-item-row)
      (order-item-row-id order-item-row))

__current-elements__  
The list of current elements.

    (list
      ('order-item-subrequest 1005 "Chocolate bar" 25)
      ('order-item-subrequest 2822 "Ninja costume" 3)
      ('order-item-subrequest 6027 "Baseball hat" 2))

__current-element-id-procedure__  
A procedure that returns a current element's id.  

    (lambda (order-item-subrequest)
      (order-item-subrequest-id order-item-subrequest))

__element-changed?-procedure__  
A procedure invoked with every pair of original and current elements that have matched by id.  
It must return whether the elements are considered to have changed.

    (lambda (order-item-row order-item-subrequest) ...)

This pair would be considered to have changed :  

    ('order-item-row 1005 "Chocolate bar" 5)
    ('order-item-subrequest 1005 "Chocolate bar" 25)

This pair would not :

    ('order-item-row 2822 "Ninja costume" 3)
    ('order-item-subrequest 2822 "Ninja costume" 3)

__make-added-compare-result-element-procedure__  
A procedure invoked with every current element that was not matched.

    ('order-item-subrequest 6027 "Baseball hat" 2)

It has to return an element representing the addition.

    ('start-item-production-request 6027 2)

__make-changed-compare-result-element-procedure__  
A procedure invoked with every pair of matched elements that have changed.

    ('order-item-row 1005 "Chocolate bar" 5)
    ('order-item-subrequest 1005 "Chocolate bar" 25)

It has to return an element representing the modification.

    ('alter-item-production-request 1005 +20)

__make-unchanged-compare-result-element-procedure__  
A procedure invoked with every pair of matched elements that have not changed.

    ('order-item-row 2822 "Ninja costume" 3)
    ('order-item-subrequest 2822 "Ninja costume" 3)

It has to return an element representing the non modification.  
This would be #f in this example.

__make-deleted-compare-result-element-procedure__  
A procedure invoked with every unmatched original elements.

    ('order-item-row 6001 "Soccer ball" 1)

It has to return an element representing the element deletion.  

    (stop-item-production-request 6001 1)

__result__  
A compare results composed of the lists of added,  
changed, unchanged and deleted elements.

    (make-compare-results
      (list
        ('start-item-production-request 6027 2))
      (list
        ('alter-item-production-request 1005 +20))
      (list
        #f)
      (list
        (stop-item-production-request 6001 1)))
