
Name
----

new-shipping-address

Description
-----------

Adds a new shipping address to a customer.

Address
-------

    http://www.server.com/fcgi-bin/customers/{customer-id}/shipping-addresses

Request definition
------------------

The request is composed of the following fields:

__customer-id__  
The customer's unique identifier.  
A number between 1 and 1000000.  
Must be included in the service address.

__street__  
The street.  
A string composed of 1 to 100 characters.  
Must be present.

__city__  
The city.  
A string composed of 1 to 50 characters.  
Must be present.

__state__  
The state.  
A string composed of 1 to 50 characters.  
Must be present.

__effective-date__  
The date at which the shipping address becomes effective.  
A date string as defined by ISO 8601.  
Must be present.

Sample request
--------------

    POST /fcgi-bin/customers/1000/shipping-addresses HTTP/1.1
    Host: www.server.com

    {
      "street": "123 Sunny Street",
      "city": "Miami",
      "state": "Florida",
      "effective-date": "2010-12-31"
    }

Response definition
-------------------

The response is composed of the following fields:

__shipping-address-id__  
The shipping address unique identifier.  
A number between 1 and 1000000.  
Will always be present.

Sample Response
---------------

    HTTP/1.1 200 OK
    Content-Type: text/json; charset=utf-8
    
    {
      "shipping-address-id": 1000
    }
