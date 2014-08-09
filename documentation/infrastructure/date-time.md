
string->date
------------
Parses a date expressed under the format YYYY-MM-DD.  
The result still needs to be validated using date-valid?.  
Returns #f if the format is not matched.

__string__

    "1972-04-28"

__result__

    (make-date 1972 4 28)

string->date-time
-----------------
Parses a UTC date and time expressed under the format YYYY-MM-DDTHH:MM:SSZ.  
The result still needs to be validated using date-time-valid?.  
Returns #f if the format is not matched.

__string__

    "1972-04-28T13:54:27Z"

__result__

    (make-date-time 1972 4 28 13 54 27)

string->time*
-------------
Parses a time expressed under the format HH:MM:SS.  
The result still needs to be validated using time-valid?.  
Returns #f if the format is not matched.

The name of the procedure is starred to avoid clashing with unit posix.

__string__

    "13:54:27"

__result__

    (make-time 13 54 27)

string->day-of-week
-------------------
Parses a day of week.  
The result still needs to be validated using day-of-week-valid?.

__string__

    "Monday"

__result__

    (make-day-of-week "Monday")

integer->day-of-week
--------------------
Converts an integer to its matching day of week.  
The result still needs to be validated using day-of-week-valid?.

__integer__

    1

__result__

    (make-day-of-week "Monday")

date->string
------------
Formats a date under the format YYYY-MM-DD.

__date__

    (make-date 1972 4 28)

__result__

    "1972-04-28"

date-time->string
-----------------
Formats a UTC date and time under the format YYYY-MM-DDTHH:MM:SSZ.

__date-time__

    (make-date-time 1972 4 28 13 54 27)

__result__

    "1972-04-28T13:54:27Z"

time->string*
-------------
Formats a time under the format HH:MM:SS.

The name of the procedure is starred to avoid clashing with unit posix.

__date__

    (make-time 13 54 27)

__result__

    "13:54:27"

time->string-without-seconds
----------------------------
Formats a time under the format HH:MM.

__date__

    (make-time 13 54 27)

__result__

    "13:54"

day-of-week->string
-------------------
Formats a day of week to string.

__day-of-week__

    (make-day-of-week "Monday")

__result__

    "Monday"

day-of-week->integer
--------------------
Converts a day of week to its integer representation.  
Returns #f upon failure.

__day-of-week__

    (make-day-of-week "Monday")

__result__

    1

date-valid?
-----------
Returns whether a date is valid.  
Used to make sure the combinaison of numbers represents a valid date.

__date__

    (make-date 1972 4 28)

__result__

    #t

date-time-valid?
----------------
Returns whether a UTC date and time is valid.  
Used to make sure the combinaison of numbers represents a valid date time.

__date__

    (make-date-time 1972 4 28 13 54 27)

__result__

    #t

time-valid?
-----------
Returns whether a time is valid.  
Used to make sure the combinaison of numbers represents a valid time.

__date__

    (make-time 13 54 27)

__result__

    #t

day-of-week-valid?
------------------
Returns whether a day of week is valid.

__day-of-week__

    (make-day-of-week "Monday")

__result__

    #t

date-now
--------
Returns the current UTC date.

__result__

    (make-date 1972 4 28)

date-time-now
-------------
Returns the current UTC date time.

__result__

    (make-date-time 1972 4 28 13 54 27)

time-now
--------
Returns the current UTC time.

__result__

    (make-time 13 54 27)

date-add
--------
Adds two dates.

__first-date__

    (make-date 1972 4 28)

__second-date__

    (make-date 1 15 2)

__result__

    (make-date 1974 7 30)

date-time-add
-------------
Adds two date times.

__first-date-time__

    (make-date-time 1972 4 28 13 54 27)

__second-date-time__

    (make-date-time 1 15 2 30 2 15)

__result__

    (make-date-time 1974 7 31 19 56 42)

time-add
---------
Adds two times.

__first-time__

    (make-time 13 54 27)

__second-time__

    (make-time 30 2 15)

__result__

    (make-time 19 56 42)

date-calculate-age
------------------
Calculates an age based on a birthdate.

__birthdate__  

    (make-date 1972 4 28)

__result__  

    41.4769
