
Sample request
--------------

    PUT /api/customers HTTP/1.1
    Host: localhost

    {
      "customer-id": 1000,
      "first-name": "Alice",
      "last-name": "Alisson",
      "birthdate": "2001-12-31",
      "shipping-addresses": [
        {
          "shipping-address-id": 2000,
          "street": "456 Cloudy Boulevard",
          "city": "Seattle",
          "state": "Washington"
        }
      ]
    }

Sample response
---------------

    HTTP/1.1 204 No Content
