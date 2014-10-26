
Structure macros
----------------
This set of macros aims at:

- Simplifying the structure of the services implementation
- Providing guidelines as to how services should efficiently perform common tasks

Dependencies
------------
Make sure to include the following lines at the top of the service file:

    (use srfi-1)
    (use srfi-69)

    (declare (uses compare))
    (declare (uses hash))
    (declare (uses list))
    (declare (uses validation))

select-one
----------
Selects one database row.  
Returns #f if the row is not found.

    ;; select the customer-row
    (select-one
      (customer-row
        customers-table-select-by-customer-id
        customer-id)

      ...)

select-one-and-validate
-----------------------
Selects one database row and validates it was found.

    ;; select and validate the customer-row
    (select-one-and-validate
      (customer-row
        customers-table-select-by-customer-id
        customer-id)
      (unknown-customer-id)

      ...)

select-many
-----------
Selects many database rows.

    ;; select the customer-rows
    (select-many
      (customer-rows
        customers-table-select-by-city
        city)

      ...)

select-many-and-hash
--------------------
Selects many database rows and hashes them for efficient referencing.

    ;; select and hash the customer-rows
    (select-many-and-hash
      (customer-rows
        customers-table-select-by-city
        city)
      (customer-rows-hash-table
        hash-with-unique-numeric-keys
        customer-row-customer-id)
      (get-customer-row
        order-row-customer-id
        #f)

      ...

      ;; get one of the customer-rows
      (let ((customer-row (get-customer-row order-row))) ...)

      ...)

make-subresponses
-----------------
Makes a list of subresponses from a list of rows.  
Sorts the source list in order to provide a predictable and repeatable ordering of the subresponses.

    ;; make the get-customers-by-city
    ;; customer-subresponses
    (make-subresponses
      (customer-rows
        list-sort-by-number
        customer-row-customer-id)
      (lambda (customer-row)
        (make-get-customers-by-city-customer-subresponse
          (customer-row-first-name customer-row)
          (customer-row-last-name customer-row))))

Reference implementation for a read service
-------------------------------------------

    (define (get-customers-by-status-service sql-connection get-customers-by-status-request)

      ;; validate the request
      (validate-request get-customers-request validate-get-customers-request)

      ;; select and validate the status-row
      (select-one-and-validate
        (status-row
          status-table-select-by-status-id
          (get-customers-request-status-id get-customers-request))
        (unknown-status-id)

        ;; select the customer-rows
        (select-many
          (customer-rows
            customers-table-select-by-status-id
            (get-customers-request-status-id get-customers-request))

          ;; select and hash the address-rows
          (select-many-and-hash
            (address-rows
              address-table-select-by-customer-status-id
              (get-customers-request-status-id get-customers-request))
            (address-rows-hash-table
              hash-with-shared-numeric-keys
              address-row-customer-id)
            (get-address-rows
              customer-row-customer-id
              (list))

            ;; make the get-customers-by-status-response
            (make-get-customers-by-status-response

              ;; make the get-customers-by-status
              ;; customer-subresponses
              (make-subresponses
                (customer-rows
                  list-sort-by-number
                  customer-row-customer-id)
                (lambda (customer-row)
                  (let ((address-rows (get-address-rows customer-row)))
                    (make-get-customer-by-status-customer-subresponse
                      (customer-row-first-name customer-row)
                      (customer-row-last-name customer-row)

                      ;; make the get-customers-by-status
                      ;; address-subresponses
                      (make-subresponses
                        (address-rows
                          list-sort-by-number
                          address-row-address-id)
                        (lambda (address-row)
                          (make-get-customer-by-status-address-subresponse
                            (address-row-street address-row)
                            (address-row-city address-row)
                            (address-row-country address-row)))))))))))))

validate-duplicates
-------------------
Validates that the subrequest values do not contain duplicates.

A numbered validation error is raised if duplicates are found.  
The error symbols are concatenated around the invalid subrequests index.  
For example: address4-address-type-id-duplicate.

    ;; validate the duplicated
    ;; address-type-id
    (validate-duplicates
      (new-customer-request-addresses
        new-customer-request
        new-customer-address-subrequest-address-type-id)
      (address
        address-type-id-duplicate))

validate-references
-------------------
Validates that the subrequest values refer to existing database rows.

A numbered validation error is raised if invalid references are found.  
The error symbols are concatenated around the invalid subrequests index.  
For example: address4-address-type-id-unknown.

    ;; validate the referred
    ;; address-type-ids
    (validate-references
      (new-customer-request-addresses
        new-customer-request
        new-customer-address-subrequest-address-type-id)
      (address-type-rows
        address-type-row-address-type-id)
      (address
        address-type-id-unknown))

validate-inserted-rows
----------------------
Searches subrequests for which the specified property returns #f.

Subrequests that are matched are then passed to the validation procedure.  
This procedure must return #t only if inserting a new database row would be valid.

A numbered validation error is raised if invalid inserts are found.  
The error symbols are concatenated around the invalid subrequests index.  
For example: address4-must-be-in-home-office-country

    ;; validate the inserted address-rows
    (validate-inserted-rows
      (update-customer-request-addresses
        update-customer-request
        update-customer-address-subrequest-address-id)
      (address
        must-be-in-home-office-country)

      ;; make sure the inserted address-rows
      ;; are in the home office country
      (lambda (update-customer-address-subrequest)
        (eq?
          (update-customer-address-subrequest-country
            update-customer-address-subrequest)
          home-office-country)))

validate-updated-rows
---------------------
Matches a list of subrequests to a list of database rows.  
This is done by joining both lists on their specified property.

Database rows that are matched are then passed to the validation procedure, along with their matching subrequest.  
This procedure must return #t only if the update is valid.

A numbered validation error is raised if invalid updates are found.  
The error symbols are concatenated around the invalid subrequests index.  
For example: address4-cannot-change-country.

    ;; validate the updated address-rows
    (validate-updated-rows
      (update-customer-request-addresses
        update-customer-request
        update-customer-address-subrequest-address-id)
      (address-rows
        address-row-address-id)
      (address
        cannot-change-country)

      ;; make sure the updated address-rows
      ;; remain in the same country
      (lambda (update-customer-address-subrequest address-row)
        (eq?
          (update-customer-address-subrequest-country-id
            update-customer-address-subrequest)
          (address-row-country-id
            address-row))))

validate-deleted-rows
---------------------
Matches a list of subrequests to a list of database rows.  
This is done by joining both lists on their specified property.

Database rows that cannot be matched are then passed to the validation procedure.  
This procedure must return #t only if the delete is valid.

A validation error is raised if invalid deletes are found.

    ;; validate the deleted address-rows
    (validate-deleted-rows
      (update-customer-request-addresses
        update-customer-request
        update-customer-address-subrequest-address-id)
      (address-rows
        address-row-address-id)
      (address-cannot-delete-primary-address)

      ;; make sure the deleted address-rows
      ;; are not primary addresses
      (lambda (address-row)
        (not (address-row-is-primary address-row))))

update-modified-rows
--------------------
Matches a list of subrequests to a list of database rows.  
This is done by joining both lists on their first specified property.

Subrequests that cannot be paired are passed to the first procedure.  
This procedure must make a row that will be inserted.

The values of the other properties are then compared between the paired subrequests and rows.  
Pairs with differing values are passed to the second procedure.  
This procedure must make a row that will be updated.

The modified rows are then passed to the insert, update and delete procedures of the table.

    ;; update the modified address-rows
    (update-modified-rows
      (update-customer-request-addresses
        update-customer-request
        update-customer-address-subrequest-address-id
        update-customer-address-subrequest-street
        update-customer-address-subrequest-city)
      (address-rows
        address-row-address-id
        address-row-street
        address-row-city)
      (address-table)

      ;; makes a new address-row
      (lambda (update-customer-address-subrequest)
        (make-address-row
          ...))

      ;; makes an updated address-row
      (lambda (update-customer-address-subrequest address-row)
        (make-address-row
          ...))

      ...)

Reference implementation for a write service
--------------------------------------------

    (define (update-customer-addresses-service sql-connection update-customer-addresses-request)

      ;; validate the request
      (validate-request update-customer-addresses-request validate-update-customer-addresses-request)

      ;; validate there are no address-id duplicates
      (validate-duplicates
        (update-customer-addresses-request-addresses
          update-customer-addresses-request
          update-customer-addresses-address-subrequest-address-id)
        (address
          address-id-duplicate))

      ;; select and validate the customer-row
      (select-one-and-validate
        (customer-row
          customers-table-select-by-customer-id
          (update-customer-addresses-request-customer-id update-customer-addresses-request))
        (unknown-customer-id)

        ;; select the address-rows
        (select-many
          (address-rows
            address-table-select-by-customer-id
            (update-customer-addresses-request-customer-id update-customer-addresses-request))

          ;; validate the referred address-id
          (validate-references
            (update-customer-addresses-request-addresses
              update-customer-addresses-request
              update-customer-addresses-address-subrequest-address-id)
            (address-rows
              address-row-address-id)
            (address
              address-id-unknown))

            ;; update the modified address-rows
            (update-modified-rows
              (update-customer-addresses-request-addresses
                update-customer-addresses-request
                update-customer-addresses-address-subrequest-address-id
                update-customer-addresses-address-subrequest-street
                update-customer-addresses-address-subrequest-city
                update-customer-addresses-address-subrequest-country)
              (address-rows
                address-row-address-id
                address-row-street
                address-row-city
                address-row-country)
              (address-table)

              ;; makes an new address-row
              (lambda (update-customer-addresses-address-subrequest)
                (make-address-row
                  0
                  (update-customer-addresses-address-subrequest-street
                    update-customer-addresses-address-subrequest)
                  (update-customer-addresses-address-subrequest-city
                    update-customer-addresses-address-subrequest)
                  (update-customer-addresses-address-subrequest-country
                    update-customer-addresses-address-subrequest)))

              ;; makes an updated address-row
              (lambda (update-customer-addresses-address-subrequest address-row)
                (make-address-row
                  (address-row-address-id address-row)
                  (update-customer-addresses-address-subrequest-street
                    update-customer-addresses-address-subrequest)
                  (update-customer-addresses-address-subrequest-city
                    update-customer-addresses-address-subrequest)
                  (update-customer-addresses-address-subrequest-country
                    update-customer-addresses-address-subrequest)))))))
