
Value fields
------------

__Definition__

    (define-response get-customer-response
      (name string)
      (credit-score integer)
      (credit-limit number))

- Defines a response composed of:
 - A name of type string
 - A credit score of type integer
 - A credit limit of type number

- Supported field types:
 - boolean
 - date
 - date-time
 - day-of-week
 - integer
 - number
 - string
 - time

__Contructor and selectors__

    (define get-customer-response (make-get-customer-response "Alice" 8 5000.00))
    (get-customer-response-name get-customer-response)
    (get-customer-response-credit-score get-customer-response)
    (get-customer-response-credit-limit get-customer-response)

__Json formatting__

    (json-format-get-customer-response response json-object)

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
      (sizes list (size integer))
      (colors list (color string)))

- Defines a response composed of:
 - A list of sizes of type integer
 - A list of colors of type string

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
      (street string)
      (city string)
      (postal-code string))

- Defines a subresponse composed of:
 - A street of type string
 - A city of type string
 - A postal code of type string

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
      (suppliers list (supplier new-product-supplier-subresponse)))

    (define-response new-product-supplier-subresponse
      (name string)
      (price number))

- Defines a list of supplier subresponses composed of:
 - A name of type string
 - A price of type number

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
