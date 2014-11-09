
Sample request
--------------

    GET /api/customers/1000/shipping-addresses HTTP/1.1
    Host: localhost

Sample response
---------------

    HTTP/1.1 200 OK
    Content-Type: application/json; charset=utf-8

    {
      "shipping-addresses": [
        {
          "shipping-address-id": 2000,
          "effective-date": "2010-12-31",
          "street": "123 Sunny Street",
          "city": "Miami",
          "state": "Florida"
        },
        {
          "shipping-address-id": 2001,
          "effective-date": "2011-03-25",
          "street": "456 Cloudy Boulevard",
          "city": "Seattle",
          "state": "Washington"
        }
      ]
    }
