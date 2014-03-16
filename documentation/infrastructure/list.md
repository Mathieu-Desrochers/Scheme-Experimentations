
list-duplicates-index
---------------------
Returns the index of the elements that appear more than once in a list.  
Ignores the elements having a false value.

__elements__  
The list of elements.

    (list 2 #t "7" 2)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__result__  
The index of the duplicates.

    (0 3)

list-number-duplicates-index
----------------------------
Returns the index of the elements that appear more than once in a list.  
Used as an optimization when the elements are known to be numbers.  
Ignores the elements having a false value.

__elements__  
The list of elements.

    (list 2 6 7 2)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__result__  
The index of the duplicates.

    (0 3)

list-string-duplicates-index
----------------------------
Returns the index of the elements that appear more than once in a list.  
Used as an optimization when the elements are known to be strings.  
Ignores the elements having a false value.

__elements__  
The list of elements.

    (list "2" "6" "7" "2")

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__result__  
The index of the duplicates.

    (0 3)

list-matches-index
------------------
Returns the index of the elements in a first list whose value can be matched in a second list.  
Ignores the elements having a false value.

__first-elements__  
The first list of elements.

    (list 1 #t "3" 4)

__first-element-value-procedure__  
A procedure that returns the value of a first list element.

    identity

__second-elements__  
The second list of elements.

    (list 9 #f 7 "6" 5 4 "3")

__second-element-value-procedure__  
A procedure that returns the value of a second list element.

    identity

__result__  
The index of the matched elements.

    (2 3)

list-non-matches-index
----------------------
Returns the index of the elements in a first list whose value cannot be matched in a second list.  
Ignores the elements having a false value.

__first-elements__  
The first list of elements.

    (list 1 #t "3" 4)

__first-element-value-procedure__  
A procedure that returns the value of a first list element.

    identity

__second-elements__  
The second list of elements.

    (list 9 #f 7 "6" 5 4 "3")

__second-element-value-procedure__  
A procedure that returns the value of a second list element.

    identity

__result__  
The index of the unmatched elements.

    (0 1)

list-number-matches-index
-------------------------
Returns the index of the elements in a first list whose value can be matched in a second list.  
Used as an optimization when all the elements are known to be numbers.  
Ignores the elements having a false value.

__first-elements__  
The first list of elements.

    (list 1 2 3 4)

__first-element-value-procedure__  
A procedure that returns the value of a first list element.

    identity

__second-elements__  
The second list of elements.

    (list 9 8 7 6 5 4 3)

__second-element-value-procedure__  
A procedure that returns the value of a second list element.

    identity

__result__  
The index of the matched elements.

    (2 3)

list-number-non-matches-index
-----------------------------
Returns the index of the elements in a first list whose value cannot be matched in a second list.  
Used as an optimization when all the elements are known to be numbers.  
Ignores the elements having a false value.

__first-elements__  
The first list of elements.

    (list 1 2 3 4)

__first-element-value-procedure__  
A procedure that returns the value of a first list element.

    identity

__second-elements__  
The second list of elements.

    (list 9 8 7 6 5 4 3)

__second-element-value-procedure__  
A procedure that returns the value of a second list element.

    identity

__result__  
The index of the unmatched elements.

    (0 1)

list-string-matches-index
-------------------------
Returns the index of the elements in a first list whose value can be matched in a second list.  
Used as an optimization when all the elements are known to be strings.  
Ignores the elements having a false value.

__first-elements__  
The first list of elements.

    (list "1" "2" "3" "4")

__first-element-value-procedure__  
A procedure that returns the value of a first list element.

    identity

__second-elements__  
The second list of elements.

    (list "9" "8" "7" "6" "5" "4" "3")

__second-element-value-procedure__  
A procedure that returns the value of a second list element.

    identity

__result__  
The index of the matched elements.

    (2 3)

list-string-non-matches-index
-----------------------------
Returns the index of the elements in a first list whose value cannot be matched in a second list.  
Used as an optimization when all the elements are known to be strings.  
Ignores the elements having a false value.

__first-elements__  
The first list of elements.

    (list "1" "2" "3" "4")

__first-element-value-procedure__  
A procedure that returns the value of a first list element.

    identity

__second-elements__  
The second list of elements.

    (list "9" "8" "7" "6" "5" "4" "3")

__second-element-value-procedure__  
A procedure that returns the value of a second list element.

    identity

__result__  
The index of the unmatched elements.

    (0 1)

list-same-values-index
----------------------
Returns the index at which the elements of two lists share the same value.

__first-elements__  
The first list of elements.

    (list 1 2 3 4)

__first-element-value-procedure__  
A procedure that returns the value of a first list element.

    identity

__second-elements__  
The second list of elements.

    (list 1 20 3 40)

__second-element-value-procedure__  
A procedure that returns the value of a second list element.

    identity

__result__  
The index of the elements that share the same value.

    (0 2)

list-different-values-index
---------------------------
Returns the index at which the elements of two lists do not share the same value.

__first-elements__  
The first list of elements.

    (list 1 2 3 4)

__first-element-value-procedure__  
A procedure that returns the value of a first list element.

    identity

__second-elements__  
The second list of elements.

    (list 1 20 3 40)

__second-element-value-procedure__  
A procedure that returns the value of a second list element.

    identity

__result__  
The index of the elements that do not share the same value.

    (1 3)

list-sort-by-number
-------------------
Sorts a list of elements by their numeric value.

__elements__  
The list of elements.

    (list 5 3 2 8)

__element-sort-value-procedure__  
A procedure that returns the numeric value of an element.

    identity

__result__  
The list of sorted elements.

    (list 2 3 5 8)

list-sort-by-string
-------------------
Sorts a list of elements by their string value.

__elements__  
The list of elements.

    (list "e" "c" "b" "h")

__element-sort-value-procedure__  
A procedure that returns the string value of an element.

    identity

__result__  
The list of sorted elements.

    (list "b" "c" "e" "h")

list-sort-by-date
-----------------
Sorts a list of elements by their date value.

__elements__  
The list of elements.

    (list
      (make-date 2005 5 5)
      (make-date 2003 3 3)
      (make-date 2002 2 2)
      (make-date 2008 8 8))

__element-sort-value-procedure__  
A procedure that returns the date value of an element.

    identity

__result__  
The list of sorted elements.

    (list
      (make-date 2002 2 2)
      (make-date 2003 3 3)
      (make-date 2005 5 5)
      (make-date 2008 8 8))

list-sort-by-day-of-week-and-time
---------------------------------
Sorts a list of elements by their day of week and time values.

__elements__  
The list of elements.

    (list
      (cons (make-day-of-week "Wednesday") (make-time 5 5 5))
      (cons (make-day-of-week "Wednesday") (make-time 3 3 3))
      (cons (make-day-of-week "Sunday") (make-time 2 2 2))
      (cons (make-day-of-week "Sunday") (make-time 8 8 8)))

__element-day-of-week-sort-value-procedure__  
A procedure that returns the day of week value of an element.

    car

__element-time-sort-value-procedure__  
A procedure that returns the time value of an element.

    cadr

__result__  
The list of sorted elements.

    (list
      (cons (make-day-of-week "Sunday") (make-time 2 2 2))
      (cons (make-day-of-week "Sunday") (make-time 8 8 8))
      (cons (make-day-of-week "Wednesday") (make-time 3 3 3))
      (cons (make-day-of-week "Wednesday") (make-time 5 5 5)))

list-is-consecutive-sequence
----------------------------
Returns whether the elements in a list form a consecutive sequence that starts from one.

__elements__  
The list of elements.

    (list 1 2 3)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__result__  
Whether the elements form a consecutive sequence.

    #t

list-minimum-element
--------------------
Returns the element in a list that has the minimum value.

__elements__  
The list of elements.

    (list 1 2 3)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__result__  
The element that has the minimum value.

    1

list-maximum-element
--------------------
Returns the element in a list that has the maximum value.

__elements__  
The list of elements.

    (list 1 2 3)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__result__  
The element that has the maximum value.

    3

list-sum
--------
Returns the sum of the elements value.

__elements__  
The list of elements.

    (list 1 2 3)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__result__  
The sum of the elements value.

    6

list-filtered-count
-------------------
Returns the number of elements whose value matches a filter.

__elements__  
The list of elements.

    (list 1 2 3 4 5)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__filter-procedure__  
A procedure that returns whether the value of an element is filtered.

    even?

__result__  
The number of filtered elements.

    2

list-filtered-index
-------------------
Returns the index of the elements whose value matches a filter.

__elements__  
The list of elements.

    (list 1 2 3 4 5)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__filter-procedure__  
A procedure that returns whether the value of an element is filtered.

    even?

__result__  
The index of the filtered elements.

    (1 3)

list-value-last-index
---------------------
Returns the last index of a value in a list.

__elements__  
The list of elements.

    (list 1 5 5 8)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__value__  
The searched value.

    5

__result__  
The last index of the value.

    2

list-keep-lowest-numbers
------------------------
Keeps only the lowest numbers in a list.

__elements__  
The list of elements.

    (list 8 1 5 8)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__count__  
The number of elements to keep.

    2

__result__  
The filtered list.

    (#f 1 5 #f)

list-remove-at-indexes
----------------------
Removes the elements at the given indexes.

__elements__  
The list of elements.

    (list 1 2 3)

__indexes__  
The indexes of the elements to remove.

    (list 0 1)

__result__  
The filtered list.

    (3)

list-matching-pairs-index
-------------------------
Returns the index of elements that can be paired  
with another element having a matching key.

__elements__  
The list of elements.

    (list 1 2 3)

__element-key-procedure__  
A procedure that returns the key of an element.

    even?

__element-matching-key-procedure__  
A procedure that returns the matching key of an element.

    odd?

__result__  
The index of the matched elements.

    (0 1)

list-remove-matching-pairs
--------------------------
Removes the elements that can be paired  
with another element having a matching key.

__elements__  
The list of elements.

    (list 1 2 3)

__element-key-procedure__  
A procedure that returns the key of an element.

    even?

__element-matching-key-procedure__  
A procedure that returns the matching key of an element.

    odd?

__result__  
The filtered elements.

    (3)

list-value-indexes
------------------
Returns the index of the elements having a given value.

__elements__  
The list of elements.

    (list 1 2 2 3)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__value__  
The searched value.

    2

__result__  
The index of the matched elements.

    (1 2)

list-remove-value
-----------------
Removes the elements having a given value.

__elements__  
The list of elements.

    (list 1 2 2 3)

__element-value-procedure__  
A procedure that returns the value of an element.

    identity

__value__  
The searched value.

    2

__result__  
The filtered elements.

    (1 3)
