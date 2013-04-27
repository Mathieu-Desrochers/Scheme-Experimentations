
Sample request
--------------

    POST /api/customers/1000/shipping-addresses HTTP/1.1
    Host: localhost

    {
      "effective-date": "2011-03-25",
      "street": "456 Cloudy Boulevard",
      "city": "Seattle",
      "state": "Washington"
    }

Sample response
---------------

    HTTP/1.1 200 OK
    Content-Type: text/json; charset=utf-8
    
    {
      "shipping-address-id": 2001
    }
