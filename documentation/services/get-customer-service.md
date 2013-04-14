
Name
----

get-customer

Description
-----------

Returns the details about a single customer.

Address
-------

    http://www.server.com/fcgi-bin/customers/{customer-id}

Request definition
------------------

The request is composed of the following fields:

__customer-id__  
The customer's unique identifier.  
Must be a number between 1 and 1000000.  
Must be included in the service address.

Sample request
--------------

    GET /fcgi-bin/customers/1000 HTTP/1.1
    Host: www.server.com

Response definition
-------------------

The response is composed of the following fields:

__first-name__  
The customer's first name.  
A string composed of 1 to 50 characters.  
Will always be present.  
  
__last-name__  
The customer's last name.  
A string composed of 1 to 50 characters.  
Will always be present.

__is-vip__  
Whether the customer is vip.  
A boolean value.  
Will always be present.

__shipping-address__  
The customer's most recent effective shipping address.  
See the get-shipping-addresses service for the complete list.  
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
      "first-name": "Alice",
      "last-name": "Anderson",
      "is-vip": true,
      "shipping-address": {
        "street": "123 Sunny Street",
        "city": "Miami",
        "state": "Florida"
      }
    }
