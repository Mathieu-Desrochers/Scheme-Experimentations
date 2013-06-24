
Sample request
--------------

    PUT /api/customers/1000/shipping-addresses/2000 HTTP/1.1
    Host: localhost

    {
      "effective-date": "2001-08-30",
      "street": "123 Sunny Boulevard",
      "city": "Miami",
      "state": "Florida"
    }

Sample response
---------------

    HTTP/1.1 204 No Content
