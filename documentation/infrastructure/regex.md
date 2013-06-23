
regex-match
-----------
Matches a string against a regular expression.

__pattern__  
The pattern to match.

    "^customers/(\\d{1,6})/(.*)$"

__string__  
The input string.

    "customers/1234/delete"

__result__  
A list containing the captured substring, and the values captured by the capturing groups.  
Returns an empty list if the pattern was not matched.

    (list
      "customers/1234"
      "1234"
      "delete")
