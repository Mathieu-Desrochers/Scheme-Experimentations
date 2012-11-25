
(declare (uses sql))
(declare (uses validation))
(declare (uses customer-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service definition

;; encapsulates a request
(define-record new-customer-request first-name last-name addresses birthdate numbers)
(define-record new-customer-request-address address city state)
(define-record new-customer-request-birthdate year month day)

;; validates a request
(define (validate-new-customer-request new-customer-request)
  (let ((first-name (new-customer-request-first-name new-customer-request))
        (last-name (new-customer-request-last-name new-customer-request))
        (addresses (new-customer-request-addresses new-customer-request))
        (birthdate (new-customer-request-birthdate new-customer-request))
        (numbers (new-customer-request-numbers new-customer-request)))
    (append
      (if (not (validate-string first-name))
        (list (cons 'invalid-first-name first-name))
        '())
      (if (not (validate-string last-name))
        (list (cons 'invalid-last-name last-name))
        '())
      (if (not (validate-list addresses))
        (list (cons 'invalid-addresses addresses))
        (concatenate
          (map
            validate-new-customer-request-address
            addresses)))
      (if (not birthdate)
        (list (cons 'invalid-birthdate birthdate))
        (validate-new-customer-request-birthdate birthdate))
      (if (not (validate-list numbers))
        (list (cons 'invalid-numbers numbers))
        (concatenate
          (map
            (lambda (number)
              (if (not (validate-integer number))
                (list (cons 'invalid-number number))
                '()))
            numbers))))))

;; validates a request address
(define (validate-new-customer-request-address new-customer-request-address)
  (let ((address (new-customer-request-address-address new-customer-request-address))
        (city (new-customer-request-address-city new-customer-request-address))
        (state (new-customer-request-address-state new-customer-request-address)))
    (append
      (if (not (validate-string address))
        (list (cons 'invalid-address-address address))
        '())
      (if (not (validate-string city))
        (list (cons 'invalid-address-city city))
        '())
      (if (not (validate-string state))
        (list (cons 'invalid-address-state state))
        '()))))

;; validates a request birthdate
(define (validate-new-customer-request-birthdate new-customer-request-birthdate)
  (let ((year (new-customer-request-birthdate-year new-customer-request-birthdate))
        (month (new-customer-request-birthdate-month new-customer-request-birthdate))
        (day (new-customer-request-birthdate-day new-customer-request-birthdate)))
    (append
      (if (not (validate-integer year))
        (list (cons 'invalid-birthdate-year year))
        '())
      (if (not (validate-integer month))
        (list (cons 'invalid-birthdate-month month))
        '())
      (if (not (validate-integer day))
        (list (cons 'invalid-birthdate-day day))
        '()))))

;; encapsulates a response
(define-record new-customer-response customer-id)

;; executes a request
(define (new-customer-service new-customer-request)
  (with-sql-connection "database.db"
    (lambda (sql-connection)
      (let ((customer-row
              (make-customer-row
                0
                (new-customer-request-first-name new-customer-request)
                (new-customer-request-last-name new-customer-request))))
        (customer-table-insert sql-connection customer-row)
        (make-new-customer-response
          (customer-row-customer-id customer-row))))))

(display
  (validate-new-customer-request
    (make-new-customer-request
      11
      "Desrochers"
      (list
        (make-new-customer-request-address 1 2 3)
        (make-new-customer-request-address "address" 23 "state"))
      (make-new-customer-request-birthdate 2000 14 "a")
      (list 1 "a" "3"))))
