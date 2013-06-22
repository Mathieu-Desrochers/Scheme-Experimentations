
string->date
------------
Parses a date expressed under the format YYYY-MM-DD.  
The result still needs to be validated using date-valid?.  
Returns #f if the format cannot be matched.

__string__

    "1972-04-28"

__result__

    (make-date 1972 4 28)


string->date-time
-----------------
Parses a UTC date and time expressed under the format YYYY-MM-DDTHH:MM:SSZ.  
The result still needs to be validated using date-time-valid?.  
Returns #f if the format cannot be matched.

__string__

    "1972-04-28T13:54:27Z"

__result__

    (make-date-time 1972 4 28 13 54 27)

string->date-time-without-seconds
---------------------------------
Parses a UTC date and time expressed under the format YYYY-MM-DDTHH:MMZ.  
The result still needs to be validated using date-time-without-seconds-valid?.  
Returns #f if the format cannot be matched.

__string__

    "1972-04-28T13:54Z"

__result__

    (make-date-time-without-seconds 1972 4 28 13 54)

string->time
------------
Parses a time expressed under the format HH:MM:SS.  
The result still needs to be validated using time-valid?.  
Returns #f if the format cannot be matched.

__string__

    "13:54:27"

__result__

    (make-time 13 54 27)

string->time-without-seconds
----------------------------
Parses a time expressed under the format HH:MM.  
The result still needs to be validated using time-without-seconds-valid?.  
Returns #f if the format cannot be matched.

__string__

    "13:54"

__result__

    (make-time-without-seconds 13 54)

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

date-time-without-seconds->string
---------------------------------
Formats a UTC date and time under the format YYYY-MM-DDTHH:MMZ.

__date-time-without-seconds__

    (make-date-time-without-seconds 1972 4 28 13 54)

__result__

    "1972-04-28T13:54Z"

time->string
------------
Formats a time under the format HH:MM:SS.

__date__

    (make-time 13 54 27)

__result__

    "13:54:27"

time-without-seconds->string
----------------------------
Formats a time under the format HH:MM.

__time-without-seconds__

    (make-time-without-seconds 13 54)

__result__

    "13:54"

day-of-week->string
-------------------
Formats a day of week.

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
Will return #f if any number is not in its allowed range.

__date__

    (make-date 1972 4 28)

__result__

    #t

date-time-valid?
----------------
Returns whether a UTC date and time is valid.  
Will return #f if any number is not in its allowed range.

__date__

    (make-date-time 1972 4 28 13 54 27)

__result__

    #t

date-time-without-seconds-valid?
--------------------------------
Returns whether a UTC date and time without seconds is valid.  
Will return #f if any number is not in its allowed range.

__date__

    (make-date-time-without-seconds 1972 4 28 13 54)

__result__

    #t

time-valid?
-----------
Returns whether a time is valid.  
Will return #f if any number is not in its allowed range.

__date__

    (make-time 13 54 27)

__result__

    #t

time-without-seconds-valid?
---------------------------
Returns whether a time without seconds is valid.  
Will return #f if any number is not in its allowed range.

__date__

    (make-time-without-seconds 13 54)

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
Returns the current UTC date and time.

__result__

    (make-date-time 1972 4 28 13 54 27)

time-now
--------
Returns the current UTC time.

__result__

    (make-time 13 54 27)

date-add-years
--------------
Adds years to a date.

__date__

    (make-date 1972 4 28)

__years__

    5

__result__

    (make-date 1977 4 28)

date-add-months
---------------
Adds months to a date.

__date__

    (make-date 1972 4 28)

__months__

    14

__result__

    (make-date 1973 6 28)

date-add-days
-------------
Adds days to a date.

__date__

    (make-date 1972 4 28)

__days__

    370

__result__

    (make-date 1973 5 3)

date-time-add-years
-------------------
Adds years to a date and time.

__date-time__

    (make-date-time 1972 4 28 13 54 27)

__years__

    5

__result__

    (make-date-time 1977 4 28 13 54 27)

date-time-add-months
--------------------
Adds months to a date and time.

__date-time__

    (make-date-time 1972 4 28 13 54 27)

__months__

    14

__result__

    (make-date-time 1973 6 28 13 54 27)

date-time-add-days
------------------
Adds days to a date and time.

__date-time__

    (make-date-time 1972 4 28 13 54 27)

__days__

    370

__result__

    (make-date-time 1973 5 3 13 54 27)

date-time-add-hours
-------------------
Adds hours to a date and time.

__date-time__

    (make-date-time 1972 4 28 13 54 27)

__hours__

    15

__result__

    (make-date-time 1972 4 29 4 54 27)

date-time-add-minutes
---------------------
Adds minutes to a date and time.

__date-time__

    (make-date-time 1972 4 28 13 54 27)

__minutes__

    25

__result__

    (make-date-time 1972 4 28 14 19 27)

date-time-add-seconds
---------------------
Adds seconds to a date and time.

__date-time__

    (make-date-time 1972 4 28 13 54 27)

__seconds__

    62

__result__

    (make-date-time 1972 4 28 13 55 29)

time-add-hours
--------------
Adds hours to a time.

__time__

    (make-time 13 54 27)

__hours__

    15

__result__

    (make-time 4 54 27)

time-add-minutes
----------------
Adds minutes to a time.

__time__

    (make-time 13 54 27)

__minutes__

    25

__result__

    (make-time 14 19 27)

time-add-seconds
----------------
Adds seconds to a time.

__time__

    (date-time 1972 4 28 13 54 27)

__seconds__

    62

__result__

    (make-time 13 55 29)
