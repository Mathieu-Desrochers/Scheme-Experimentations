
(declare (unit json-parse))

(declare (uses exceptions))
(declare (uses json))

;; parses a value
(define (json-parse-value json-object property-name)
  (let ((json-object-property (json-object-property json-object property-name)))
    (if json-object-property
      (json-object-value json-object-property)
      #f)))

;; parses a value list
(define (json-parse-value-list json-object property-name)
  (let ((json-object-property (json-object-property json-object property-name)))
    (if json-object-property
      (if (json-object-type-array? json-object-property)
        (let ((json-object-array-elements (json-object-array-elements json-object-property)))
          (map
            json-object-value
            json-object-array-elements))
        (json-object-value
          json-object-property))
      #f)))

;; parses a subrequest
(define (json-parse-subrequest json-object property-name json-parse-subrequest-procedure)
  (let ((json-object-property (json-object-property json-object property-name)))
    (if json-object-property
      (if (json-object-type-object? json-object-property)
        (json-parse-subrequest-procedure
          json-object-property)
        (json-object-value
          json-object-property))
      #f)))

;; parses a subrequest list
(define (json-parse-subrequest-list json-object property-name json-parse-subrequest-procedure)
  (let ((json-object-property (json-object-property json-object property-name)))
    (if json-object-property
      (if (json-object-type-array? json-object-property)
        (let ((json-object-array-elements (json-object-array-elements json-object-property)))
          (map
            (lambda (json-object-array-element)
              (if (json-object-type-object? json-object-array-element)
                (json-parse-subrequest-procedure
                  json-object-array-element)
                (json-object-value
                  json-object-array-element)))
            json-object-array-elements))
        (json-object-value
          json-object-property))
      #f)))

;; parses a request
(define (json-parse-request string json-parse-request-procedure)
  (hide-exceptions
    (lambda ()
      (with-parsed-json-object string
        (lambda (json-object)
          (json-parse-request-procedure json-object))))))
