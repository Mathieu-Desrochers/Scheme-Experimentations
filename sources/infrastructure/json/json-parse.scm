
(declare (unit json-parse))

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
      (let ((json-object-array-elements (json-object-array-elements json-object-property)))
        (if json-object-array-elements
          (map
            json-object-value
            json-object-array-elements)
          #f))
      #f)))

;; parses a subrequest
(define (json-parse-subrequest json-object property-name json-parse-subrequest-procedure)
  (let ((json-object-property (json-object-property json-object property-name)))
    (if json-object-property
      (json-parse-subrequest-procedure json-object-property)
      #f)))

;; parses a subrequest list
(define (json-parse-subrequest-list json-object property-name json-parse-subrequest-procedure)
  (let ((json-object-property (json-object-property json-object property-name)))
    (if json-object-property
      (let ((json-object-array-elements (json-object-array-elements json-object-property)))
        (if json-object-array-elements
          (map
            json-parse-subrequest-procedure
            json-object-array-elements)
          #f))
      #f)))
