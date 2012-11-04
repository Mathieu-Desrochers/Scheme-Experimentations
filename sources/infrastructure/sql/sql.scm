
(declare (uses sqlite-ffi))

(use srfi-1)

; open the database
(let ((sqlite3** (malloc-sqlite3*)))

  (display "sqlite3-open: ")
  (display (sqlite3-open "customers.db" sqlite3**))
  (display "\n")

  ; create the customers table
  (let ((sqlite3-stmt** (malloc-sqlite3-stmt*)))

    ; prepare the sql statement
    (display "sqlite3-prepare-v2: ")
    (display (sqlite3-prepare-v2 (indirect-sqlite3** sqlite3**) "CREATE TABLE IF NOT EXISTS Customers (CustomerID INT, Age REAL, Name TEXT, Address TEXT);" -1 sqlite3-stmt** #f))
    (display "\n")

    ; execute the sql statement
    (display "sqlite3-step: ")
    (display (sqlite3-step (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (display "\n")

    ; finalize the sql statement
    (display "sqlite3-finalize: ")
    (display (sqlite3-finalize (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (free-sqlite3-stmt* sqlite3-stmt**)
    (display "\n"))

  ; insert a customer row
  (let ((sqlite3-stmt** (malloc-sqlite3-stmt*)))

    ; prepare the sql statement
    (display "sqlite3-prepare-v2: ")
    (display (sqlite3-prepare-v2 (indirect-sqlite3** sqlite3**) "INSERT INTO Customers VALUES (?1, ?2, ?3, ?4);" -1 sqlite3-stmt** #f))
    (display "\n")

    ; bind the sql parameters
    (sqlite3-bind-int (indirect-sqlite3-stmt** sqlite3-stmt**) 1 1)
    (sqlite3-bind-double (indirect-sqlite3-stmt** sqlite3-stmt**) 2 1.2)
    (sqlite3-bind-text (indirect-sqlite3-stmt** sqlite3-stmt**) 3 "A" -1 sqlite3-transient)
    (sqlite3-bind-null (indirect-sqlite3-stmt** sqlite3-stmt**) 4)

    ; execute the sql statement
    (display "sqlite3-step: ")
    (display (sqlite3-step (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (display "\n")

    ; finalize the sql statement
    (display "sqlite3-finalize: ")
    (display (sqlite3-finalize (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (free-sqlite3-stmt* sqlite3-stmt**)
    (display "\n"))

  ; select the customer row
  (let ((sqlite3-stmt** (malloc-sqlite3-stmt*)))

    ; prepare the sql statement
    (display "sqlite3-prepare-v2: ")
    (display (sqlite3-prepare-v2 (indirect-sqlite3** sqlite3**) "SELECT * FROM Customers;" -1 sqlite3-stmt** #f))
    (display "\n")

    ; execute the sql statement
    (display "sqlite3-step: ")
    (display (sqlite3-step (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (display "\n")

    ; get the number of returned columns
    (display "sqlite3-column-count: ")
    (define columns-count (sqlite3-column-count (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (display columns-count)
    (display "\n")

    ; get the types of the columns
    (map
      (lambda (column-index)
        (display "sqlite3-column-type: ")
        (display (sqlite3-column-type (indirect-sqlite3-stmt** sqlite3-stmt**) column-index))
        (display "\n"))
      (iota columns-count))

    ; get the value of the integer column
    (display "sqlite3-column-int: ")
    (display (sqlite3-column-int (indirect-sqlite3-stmt** sqlite3-stmt**) 0))
    (display "\n")

    ; get the value of the double column
    (display "sqlite3-column-double: ")
    (display (sqlite3-column-double (indirect-sqlite3-stmt** sqlite3-stmt**) 1))
    (display "\n")

    ; get the value of the text column
    (display "sqlite3-column-text: ")
    (display (sqlite3-column-text (indirect-sqlite3-stmt** sqlite3-stmt**) 2))
    (display "\n")

    ; finalize the sql statement
    (display "sqlite3-finalize: ")
    (display (sqlite3-finalize (indirect-sqlite3-stmt** sqlite3-stmt**)))
    (free-sqlite3-stmt* sqlite3-stmt**)
    (display "\n"))

  ; close the database
  (display "sqlite3-close-v2: ")
  (display (sqlite3-close-v2 (indirect-sqlite3** sqlite3**)))
  (free-sqlite3* sqlite3**)
  (display "\n"))
