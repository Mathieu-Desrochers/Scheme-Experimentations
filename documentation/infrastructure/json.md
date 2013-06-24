
with-parsed-json-object
-----------------------
Invokes a procedure with a parsed json object.  
Throws an exception if the string cannot be parsed.

__string__  
A json string.

    "{\"Name\": \"Alice\"}"

__procedure__  
A procedure invoked with the parsed json-object.

    (lambda (json-object) ...)

json-object-property
--------------------
Returns a property of a json object.

__json-object__  
A json object of type object.  
Returns #f if the json object is of the wrong type, or if the property is not found.  
See also json-object-type-object?.

    {"Name": "Alice"} <-- json-object

__property-name__  
The name of a property.

	"Name"

__result__  
A json-object representing the property.

            json-object
                |
                v
    {"Name": "Alice"}

json-object-value
-----------------
Returns the value of a json object.

__json-object__  
A json object of type value.  
Returns #f if the json object is of the wrong type.  
See also json-object-type-value?.

            json-object
                |
                v
    {"Name": "Alice"}

__result__  
The json object's value.

    "Alice"

json-object-array-elements
--------------------------
Returns the array elements of a json object.

__json-object__  
A json object of type array.  
Returns #f if the json object is of the wrong type.  
See also json-object-type-array?.

    ["One", "Two", "Three"] <-- json-object

__result__  
The json objects in the array.

          json-objects
              |
       +------+-------+
       v      v       v
    ["One", "Two", "Three"]

json-object-type-object?
------------------------
Returns whether a json object represents an object.

__json-object__  

    {"Name": "Alice"} <-- json-object

__result__

    #t

json-object-type-value?
-----------------------
Returns whether a json object represents a value.

__json-object__

            json-object
                |
                v
    {"Name": "Alice"}

__result__

    #t

json-object-type-array?
-----------------------
Returns whether a json object represents an array.

__json-object__

    ["One", "Two", "Three"] <-- json-object

__result__

    #t

with-new-json-object
--------------------
Invokes a procedure with a json object representing an object.

__procedure__  
The procedure to invoke.

    (lambda (json-object) ...)
                 |
                 v
                { }

with-new-json-object-from-value
-------------------------------
Invokes a procedure with a json object representing a value.

__value__  
The value of the json object.

    "Alice"

__procedure__  
The procedure to invoke.

    (lambda (json-object) ...)
                 |
                 v
              "Alice"

with-new-json-object-array
--------------------------
Invokes a procedure with a json object representing an array.

__procedure__  
The procedure to invoke.

    (lambda (json-object) ...)
                 |
                 v
                [ ]

json-object-property-set!
-------------------------
Adds a property to a json object of type object.  
Throws an exception if the json object is of the wrong type.  
See also json-object-type-object?.

__json-object__  
The json object to which the property is added.

    {} <-- json-object

__property-name__  
The name of the property.

    "Name"

__json-object-value__  
The json object representing the property's value.

    "Alice" <-- json-object-value

__side effect__  
The property is added to the json-object.

    {"Name": "Alice"} <-- json-object

json-object-array-append!
-------------------------
Adds an element to a json object of type array.  
Throws an exception if the json object is of the wrong type.  
See also json-object-type-array?.

__json-object__  
The json object to which the element is added.

    [] <-- json-object

__json-object-element__  
The json object representing the element's value.

    "Alice" <-- json-object-element

__side effect__  
The element is added to the json-object.

    ["Alice"] <-- json-object

json-object->string
-------------------
Serializes a json-object to string.

__json-object__

    {"Name": "Alice"} <-- json-object

__result__

    "{\"Name\": \"Alice\"}"

is-empty-json-object-string?
----------------------------
Returns whether a string represents an empty json object.

__string__

    "{}"

__result__

    #t
