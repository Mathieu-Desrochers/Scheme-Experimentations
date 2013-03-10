
Value fields
------------

__Definition__

    (define-response get-customer-response
      (name)
      (credit-score)
      (credit-limit))

- A name
- A credit score
- A credit limit

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

Subresponse fields
------------------

__Definition__

    (define-response new-customer-response
      (address new-customer-address-subresponse))

    (define-response new-customer-address-subresponse
      (street)
      (city)
      (postal-code))

- An address subresponse composed of:
 - A street
 - A city
 - A postal code

__Json representation__

    {
      "address":
      {
        "street": "123 Sunny Street",
        "city": "Montreal",
        "postal-code": "H2J 4R1"
      }
    }

Subresponse list fields
-----------------------

__Definition__

    (define-response new-product-response
      (suppliers list new-product-supplier-subresponse))

    (define-response new-product-supplier-subresponse
      (name)
      (price))

- A list of supplier subresponses composed of:
 - A name
 - A price

__Json representation__

    {
      "suppliers":
      [
        {
          "name": "Supplier1",
          "price": 100.00
        },
        {
          "name": "Supplier2",
          "price": 200.00
        }
      ]
    }
