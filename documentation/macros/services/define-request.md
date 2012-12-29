
Simple fields
-------------

__Definition__

    (define-request new-customer-request
      (name string #t 1 100)
      (credit-score integer #t 1 10)
      (credit-limit number #f 0.00 10000.00))

- A required name of type string between 1 and 100 characters long
- A required credit score of type integer between the values 1 and 10
- An optional credit limit of type number between the values 0.00 and 10000.00

__Contructor and selectors__

    (define new-customer-request (make-new-customer-request "Alice" 8 5000.00))
    (new-customer-request-name new-customer-request)
    (new-customer-request-credit-score new-customer-request)
    (new-customer-request-credit-limit new-customer-request)

- "Alice"
- 8
- 5000.

__Validation__

    (validate-new-customer-request
      (make-new-customer-request "Alice" #f 12000.00))

- ((invalid-credit-score . #f) (invalid-credit-limit . 12000.))

__Parsing__

    (parse-new-customer-request
      (string-append
        "{"
          "\"name\": \"Alice\","
          "\"credit-score\": \"8\","
          "\"credit-limit\": \"5000.00\""
        "}"))

List fields
-----------

__Definition__

    (define-request new-product-request
      (sizes list #t 1 5 (size integer #t 1 25))
      (colors list #t 1 10 (color string #t 1 100)))

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

    (define-request new-customer-request
      (address subrequest #t new-customer-address-subrequest))

    (define-request new-customer-address-subrequest
      (street string #t 1 100)
      (city string #t 1 100)
      (postal-code string #f 1 10))

- A required address subrequest composed of:
 - A required street of type string between 1 and 100 characters long
 - A required city of type string between 1 and 100 characters long
 - An optional postal code of type string between 1 and 10 characters long

__Request validation__

    (validate-new-customer-request
      (make-new-customer-request #f))

- ((invalid-address . #f))

__Subrequest validation__

    (validate-new-customer-request
      (make-new-customer-request
        (make-new-customer-address-subrequest
          ""
          "Montreal"
          "H2J 4R1 A124")))

- ((invalid-street . "") (invalid-postal-code "H2J 4R1 A124"))

Subrequest list fields
----------------------

__Definition__

    (define-request new-product-request
      (suppliers list #t 1 3 (supplier subrequest #t new-product-supplier-subrequest)))

    (define-request new-product-supplier-subrequest
      (name string #t 1 100)
      (price number #t 0.00 100000.00))

- A required list of 1 to 3 supplier subrequests composed of:
 - A required name of type string between 1 and 100 characters long
 - A required price of type number between 0.00 and 100000.00

__List validation__

    (validate-new-product-request
      (make-new-product-request
        (list
          (make-new-product-supplier-subrequest "Supplier1" 100.00)
          (make-new-product-supplier-subrequest "Supplier2" 200.00)
          (make-new-product-supplier-subrequest "Supplier3" 300.00)
          (make-new-product-supplier-subrequest "Supplier4" 400.00))))

- ((invalid-suppliers-length . 4))

__Elements validation__

    (validate-new-product-request
      (make-new-product-request
        (list
          "Supplier1"
          (make-new-product-supplier-subrequest "" 200.00)
          (make-new-product-supplier-subrequest "Supplier3" 300000.00))))

- ((invalid-supplier . "Supplier1") (invalid-name . "") (invalid-price . 300000.))
