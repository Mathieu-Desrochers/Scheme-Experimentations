
Sample request
--------------

    POST /api/customers HTTP/1.1
    Host: localhost

    {
      "first-name": "Alice",
      "last-name": "Allison",
      "birthdate": "2001-12-31",
      "shipping-address":
      {
        "street": "123 Sunny Street",
        "city": "Miami",
        "state": "Florida"
      }
    }

Sample response
---------------

    HTTP/1.1 200 OK
    Content-Type: application/json; charset=utf-8

    {
      "customer-id": 1000
    }
