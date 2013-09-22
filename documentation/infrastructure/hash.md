
hash-with-unique-keys
---------------------
Hashes a set of elements that required to have unique keys.

__elements__  
The list of elements.

    (list
      (make-product-rating-row 1005 "Chocolate bar" 4)
      (make-product-rating-row 2822 "Ninja costume" 5)
      (make-product-rating-row 6001 "Soccer ball" 4))

__element-key-procedure__  
A procedure that returns an element's key.

    product-rating-row-product-id

__element-value-procedure__  
A procedure that returns an element's value.

    product-rating-row-name

__result__  
A hash table.

    [1005]: "Chocolate bar"
    [2822]: "Ninja costume"
    [6001]: "Soccer ball"

hash-with-shared-keys
---------------------
Hashes a set of elements that are permitted to share keys.

__elements__  
The list of elements.

    (list
      (make-product-rating-row 1005 "Chocolate bar" 4)
      (make-product-rating-row 2822 "Ninja costume" 5)
      (make-product-rating-row 6001 "Soccer ball" 4)
      (make-product-rating-row 7823 "Broccoli" 1))

__element-key-procedure__  
A procedure that returns an element's key.

    product-rating-row-stars-count

__element-value-procedure__  
A procedure that returns an element's value.

    product-rating-row-name

__result__  
A hash table where the value of elements  
sharing a common key are grouped into lists.

    [1]: ("Broccoli")
    [4]: ("Chocolate bar" "Soccer ball")
    [5]: ("Ninja costume")
