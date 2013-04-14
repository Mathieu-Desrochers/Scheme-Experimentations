
Name
----

new-customer

Description
-----------

Creates a new customer.

Address
-------

    http://www.server.com/fcgi-bin/customers

Request definition
------------------

The request is composed of the following fields:

__first-name__  
The customer's first name.  
A string composed of 1 to 50 characters.  
Must be present.

__last-name__  
The customer's last name.  
A string composed of 1 to 50 characters.  
Must be present.

__is-vip__  
Whether the customer is vip.  
A boolean value.  
Must be present.

__shipping-address__  
The customer's shipping address.  
Must be present.

__shipping-address / street__  
The shipping address street.  
A string composed of 1 to 100 characters.  
Must be present.

__shipping-address / city__  
The shipping address city.  
A string composed of 1 to 50 characters.  
Must be present.

__shipping-address / state__  
The shipping address state.  
A string composed of 1 to 50 characters.  
Must be present.

Sample request
--------------

    POST /fcgi-bin/customers HTTP/1.1
    Host: www.server.com

    {
      "first-name": "Alice",
      "last-name": "Anderson",
      "is-vip": false,
      "shipping-address":
      {
        "street": "123 Sunny Street",
        "city": "Miami",
        "state": "Florida"
      }
    }

Response definition
-------------------

The response is composed of the following fields:

__customer-id__  
The customer's unique identifier.  
A number between 1 and 1000000.  
Will always be present.

Sample Response
---------------

    HTTP/1.1 200 OK
    Content-Type: text/json; charset=utf-8
    
    {
      "customer-id": 1000
    }
