
(declare (unit json))
(declare (uses jansson-ffi))

;; encapsulates a json object
(define-record json-object jansson*)

;; invokes a procedure with a parsed json object
(define (with-parsed-json-object string procedure)
  (let ((jansson-error* (malloc-jansson-error)))
    (when (not jansson-error*)
      (abort "could not allocate jansson-error"))
    (handle-exceptions exception
      (begin
        (free-jansson-error jansson-error*)
        (abort exception))
      (let ((jansson* (jansson-loads string 0 jansson-error*)))
        (when (not jansson*)
          (abort "could not load json string"))
        (handle-exceptions exception
          (begin
            (jansson-decref jansson*)
            (abort exception))
          (let* ((json-object (make-json-object jansson*))
                 (procedure-result (procedure json-object)))
            (free-jansson-error jansson-error*)
            (jansson-decref jansson*)
            procedure-result))))))

;; returns the value of a json object
(define (json-object-value json-object)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-type (jansson-typeof jansson*)))
    (cond ((eq? jansson-type jansson-object) #f)
          ((eq? jansson-type jansson-array) #f)
          ((eq? jansson-type jansson-string) (jansson-string-value jansson*))
          ((eq? jansson-type jansson-integer) (jansson-integer-value jansson*))
          ((eq? jansson-type jansson-real) (jansson-real-value jansson*))
          ((eq? jansson-type jansson-true) #t)
          ((eq? jansson-type jansson-false) #f)
          ((eq? jansson-type jansson-null) #f))))

;; returns the json object of a property
(define (json-object-property json-object property-name)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-property* (jansson-object-get jansson* property-name)))
    (if jansson-property*
      (make-json-object jansson-property*)
      #f)))

;; returns the json objects for the items of an array
(define (json-object-array-elements json-object)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-type (jansson-typeof jansson*)))
    (if (eq? jansson-type jansson-array)
      (let ((jansson-array-size (jansson-array-size jansson*)))
        (map
          (lambda (index)
            (let ((jansson-element* (jansson-array-get jansson* index)))
              (make-json-object jansson-element*)))
          (iota jansson-array-size)))
      #f)))

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
(define (json-parse-subrequest json-object property-name parse-subrequest-procedure)
  (let ((json-object-property (json-object-property json-object property-name)))
    (if json-object-property
      (parse-subrequest-procedure json-object-property)
      #f)))

;; parses a subrequest list
(define (json-parse-subrequest-list json-object property-name parse-subrequest-procedure)
  (let ((json-object-property (json-object-property json-object property-name)))
    (if json-object-property
      (let ((json-object-array-elements (json-object-array-elements json-object-property)))
        (if json-object-array-elements
          (map
            parse-subrequest-procedure
            json-object-array-elements)
          #f))
      #f)))

;; invokes a procedure with a new json object
(define (with-new-json-object procedure)
  (let ((jansson* (jansson-object)))
    (when (not jansson*)
      (abort "could not create new jansson*"))
    (handle-exceptions exception
      (begin
        (jansson-decref jansson*)
        (abort exception))
      (let* ((json-object (make-json-object jansson*))
             (procedure-result (procedure json-object)))
        (jansson-decref jansson*)
        procedure-result))))

;; invokes a procedure with a new json object representing a value
(define (with-new-json-object-from-value value procedure)
  (define (create-jansson*)
    (cond ((boolean? value) (jansson-boolean value))
          ((and (integer? value) (exact? value)) (jansson-integer value))
          ((number? value) (jansson-real value))
          ((string? value) (jansson-string value))
          ((not value) (jansson-null))))
  (let ((jansson* (create-jansson*)))
    (when (not jansson*)
      (abort "could not create jansson* from value"))
    (handle-exceptions exception
      (begin
        (jansson-decref jansson*)
        (abort exception))
      (let* ((json-object (make-json-object jansson*))
             (procedure-result (procedure json-object)))
        (jansson-decref jansson*)
        procedure-result))))

;; sets a property to a json object
(define (json-object-property-set! json-object property-name json-object-value)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-value* (json-object-jansson* json-object-value))
         (json-object-set-result (jansson-object-set jansson* property-name jansson-value*)))
    (when (not (eq? json-object-set-result 0))
      (abort "could not set json object property"))))

;; serializes a json object to a string
(define (json-object->string json-object)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-dumps-result* (jansson-dumps-wrapped jansson*)))
    (when (not jansson-dumps-result*)
      (abort "could not serialize json object"))
    (let ((result (jansson-dumps-result-value jansson-dumps-result*)))
      (jansson-free-dumps-result jansson-dumps-result*)
      result)))
