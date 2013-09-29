
increment
---------
Increments a record value.

__record__  
The record to update.

  (make-customer-row 1001 "Alice" 25)

__get-procedure__  
A procedure that gets the value to be incremented.

    customer-row-age

__set-procedure__  
A procedure that sets the value to be incremented.

    customer-row-age-set!

__by__  
The number by which to increment the value.

    1

__result__  
Updates the record.

    (get-customer-row-age alice) ==> 26
