
Simple fields
=============

Definition
----------

    (define-request customer-request  
      (name string #t 1 100)  
      (credit-score integer #t 1 10)  
      (credit-limit number #f 0.00 10000.00))

- A required name of type string between 1 and 100 characters long
- A required credit score of type integer between the values 1 and 10
- An optional credit limit of type number between the values 0.00 and 10000.00

Contructor and selectors
------------------------

    (define customer-request (make-customer-request "Alice" 8 5000.00))  
    (customer-request-name customer-request)  
    (customer-request-credit-score customer-request)  
    (customer-request-credit-limit customer-request)

- "Alice"
- 8
- 5000.

Validation
----------

    (define customer-request (make-customer-request "Alice" #f 12000.00))  
    (validate-customer-request customer-request)  

- ((invalid-credit-score . #f) (invalid-credit-limit . 12000.))

List fields
===========

Definition
----------

    (define-request new-product-request  
      (sizes list #t 1 5 size integer 1 25)  
      (colors list #t 1 10 color string 1 100))

- A required list of 1 to 5 sizes of type integer between the values of 1 and 25
- A required list of 1 to 10 colors of type string between 1 and 100 characters long

List validation
---------------

    (validate-new-product-request  
      (make-new-product-request  
        (list 10 11 12 13 14 15)  
        #f))

- ((invalid-sizes-length . 6) (invalid-colors . #f))

Elements validation
-------------------

    (validate-new-product-request  
      (make-new-product-request  
        (list 10 20 30 40)  
        (list "Green" "Yellow" 1.25)))

- ((invalid-size . 30) (invalid-size . 40) (invalid-color . 1.25))

Subrequest fields
=================

Definition
----------

    (define-request customer-request  
      (address subrequest #t customer-address-subrequest))

    (define-request customer-address-subrequest  
      (street string #t 1 100)  
      (city string #t 1 100)  
      (state string #t 1 100))

- A required address subrequest composed of:
 - A required street of type string between 1 and 100 characters long
 - A required city of type string between 1 and 100 characters long
 - A required state of type string between 1 and 100 characters long
