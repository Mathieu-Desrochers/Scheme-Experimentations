
Name
----

get-shipping-addresses

Description
-----------

Returns the shipping addresses of a customer.

Address
-------

    http://www.server.com/fcgi-bin/customers/{customer-id}/shipping-addresses

Request definition
------------------

The request is composed of the following fields:

__customer-id__  
The customer's unique identifier.  
Must be a number between 1 and 1000000.  
Must be included in the service address.

Sample request
--------------

    GET /fcgi-bin/customers/1000/shipping-addresses HTTP/1.1
    Host: www.server.com

Response definition
-------------------

The response is composed of the following fields:

__shipping-addresses__  
The customer's shipping addresses.  
An array of shipping-address objects.  
Will always be present.

__shipping-address / shipping-address-id__  
The customer's unique identifier.  
A number between 1 and 1000000.  
Will always be present.

__shipping-address / street__  
The shipping address street.  
A string composed of 1 to 100 characters.  
Will always be present.

__shipping-address / city__  
The shipping address city.  
A string composed of 1 to 50 characters.  
Will always be present.

__shipping-address / state__  
The shipping address state.  
A string composed of 1 to 50 characters.  
Will always be present.

Sample response
---------------

    HTTP/1.1 200 OK
    Content-Type: text/json; charset=utf-8
    
    {
      "shipping-addresses": [
        {
          "effective-date": "2010-12-31",
          "street": "123 Sunny Street",
          "city": "Miami",
          "state": "Florida"
        },
        {
          "effective-date": "2012-12-31",
          "street": "456 Cloudy Boulevard",
          "city": "Seattle",
          "state": "Washington"
        }
      ]
    }
