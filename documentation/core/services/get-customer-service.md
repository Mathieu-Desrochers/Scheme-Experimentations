
Sample request
--------------

    GET /api/customers/1000 HTTP/1.1
    Host: localhost

Sample response
---------------

    HTTP/1.1 200 OK
    Content-Type: application/json; charset=utf-8

    {
      "customer-id": 1000,
      "first-name": "Alice",
      "last-name": "Allison",
      "is-vip": true,
      "shipping-addresses": [
        {
          "shipping-address-id": 2000,
          "street": "123 Sunny Street",
          "city": "Miami",
          "state": "Florida"
        }
      ]
    }
