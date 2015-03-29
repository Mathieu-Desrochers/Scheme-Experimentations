
Sample request
--------------

    GET /api/customers HTTP/1.1
    Host: localhost

Sample response
---------------

    HTTP/1.1 200 OK
    Content-Type: application/json; charset=utf-8

    {
      "customers": [
        {
          "customer-id": 1000,
          "first-name": "Alice",
          "last-name": "Allison"
        },
        {
          "customer-id": 1001,
          "first-name": "Bob",
          "last-name": "Bobson"
        }
      ]
    }
