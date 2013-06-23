
with-parsed-json-object
-----------------------
Invokes a procedure with a parsed json object.  
Throws an exception if the string cannot be parsed.

__string__

    {"Name": "Alice"}

__procedure__  

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

	"Name"

__result__  
A json-object representing the property's value.

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

    "Alice"

json-object-array-elements
--------------------------
Returns the elements of a json object.

__json-object__  
A json object of type array.  
Returns #f if the json object is of the wrong type.  
See also json-object-type-array?.

    ["One", "Two", "Three"] <-- json-object

__result__

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
