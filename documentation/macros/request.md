
Simple fields
-------------

__Definition__

    (define-request customer-request
      (name string #t 1 100)
      (credit-score integer #t 1 10)
      (credit-limit number #f 0.00 10000.00))

- A required name of type string between 1 and 100 characters long
- A required credit score of type integer between the values 1 and 10
- An optional credit limit of type number between the values 0.00 and 10000.00

__Contructor and selectors__

    (define customer-request (make-customer-request "Alice" 8 5000.00))
    (customer-request-name customer-request)
    (customer-request-credit-score customer-request)
    (customer-request-credit-limit customer-request)

- "Alice"
- 8
- 5000.

__Validation__

    (validate-customer-request
      (make-customer-request "Alice" #f 12000.00))

- ((invalid-credit-score . #f) (invalid-credit-limit . 12000.))

List fields
-----------

__Definition__

    (define-request new-product-request
      (sizes list #t 1 5 size integer 1 25)
      (colors list #t 1 10 color string 1 100))

- A required list of 1 to 5 sizes of type integer between the values of 1 and 25
- A required list of 1 to 10 colors of type string between 1 and 100 characters long

__List validation__

    (validate-new-product-request
      (make-new-product-request
        (list 10 11 12 13 14 15)
        #f))

- ((invalid-sizes-length . 6) (invalid-colors . #f))

__Elements validation__

    (validate-new-product-request
      (make-new-product-request
        (list 10 20 30 40)
        (list "Green" "Yellow" 1.25)))

- ((invalid-size . 30) (invalid-size . 40) (invalid-color . 1.25))

Subrequest fields
-----------------

__Definition__

    (define-request customer-request
      (address request #t customer-request-address-subrequest))

    (define-request customer-request-address-subrequest
      (street string #t 1 100)
      (city string #t 1 100)
      (postal-code string #f 1 10))

- A required address subrequest composed of:
 - A required street of type string between 1 and 100 characters long
 - A required city of type string between 1 and 100 characters long
 - An optional postal code of type string between 1 and 10 characters long

__Request validation__

    (validate-customer-request
      (make-customer-request #f))

- ((invalid-address . #f))

__Subrequest validation__

    (validate-customer-request
      (make-customer-request
        (make-customer-request-address-subrequest
          ""
          "Montreal"
          "H2J 4R1 A124")))

- ((invalid-street . "") (invalid-postal-code "H2J 4R1 A124"))
