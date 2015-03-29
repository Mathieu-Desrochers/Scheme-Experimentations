
(use srfi-1)
(use srfi-13)
(use srfi-69)

(declare (unit get-customers-service))

(declare (uses hash))
(declare (uses list))
(declare (uses customers-table))
(declare (uses validation))
(declare (uses validation-service-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request definition

(define-request get-customers-request)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response definition

(define-response get-customers-response
  (customers list (customer get-customers-customer-subresponse)))

(define-response get-customers-customer-subresponse
  (customer-id integer)
  (first-name string)
  (last-name string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; service logic

(define (get-customers-service sql-connection get-customers-request)

  ;; validate the request
  (validate-request get-customers-request validate-get-customers-request)

  ;; select the customer-rows
  (select-many
    (customer-rows
      customers-table-select-all)

    ;; make the get-customers-response
    (make-get-customers-response

      ;; make the get-customers
      ;; customer-subresponses
      (make-subresponses
        (customer-rows
          list-sort-by-number
          customer-row-customer-id)
        (lambda (customer-row)
          (make-get-customers-customer-subresponse
            (customer-row-customer-id customer-row)
            (customer-row-first-name customer-row)
            (customer-row-last-name customer-row)))))))
