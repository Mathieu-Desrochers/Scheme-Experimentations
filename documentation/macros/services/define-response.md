
Value fields
------------

__Definition__

    (define-response get-customer-response
      (name)
      (credit-score)
      (credit-limit))

- A name of type string
- A credit score of type integer
- A credit limit of type number

__Contructor and selectors__

    (define get-customer-response (make-get-customer-response "Alice" 8 5000.00))
    (get-customer-response-name get-customer-response)
    (get-customer-response-credit-score get-customer-response)
    (get-customer-response-credit-limit get-customer-response)

__Json representation__

    {
      "name": "Alice",
      "credit-score": 8,
      "credit-limit": 5000.00
    }

__Formatting__

    (format-get-customer-response get-customer-response json-object)

Value list fields
-----------------

__Definition__

    (define-response get-product-response
      (sizes list)
      (colors list))

- A list of sizes
- A list of colors

__Json representation__

    {
      "sizes": [10, 11, 12, 13, 14],
      "colors": ["Green", "Yellow", "Blue"]
    }

