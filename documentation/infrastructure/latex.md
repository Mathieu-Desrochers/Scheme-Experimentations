
latex-number->string-with-vulgar-fraction
-----------------------------------------
Formats a number to a latex string that uses vulgar fraction instructions.  
Make sure your latex document includes the nicefrac package.

__number__  
The number to format.

    12.34

__result__  
The formatted number.

    "12\\nicefrac{1}{4}"

latex-escape
------------
Escapes the latex characters in a string.

__string__  
The string to escape.

    "100.00$"

__result__  
The escaped string.

    "100.00\\$"

latex-print-pdf
---------------
Compiles the specified latex source to pdf.

__latex-source__  
The latex source.

    "\\documentclass{article}
     \\begin{document}
     ...
     \\end{document}"

__result__  
The content of the pdf file as a blob.
