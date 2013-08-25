
(use srfi-1)

(declare (unit json))

(declare (uses jansson))
(declare (uses json-intern))

;; encapsulates a json object
(define-record json-object jansson*)

;; invokes a procedure with a parsed json object
(define (with-parsed-json-object string procedure)
  (with-jansson-error*
    (lambda (jansson-error*)
      (with-loaded-jansson*
        string
        (lambda (jansson*)
          (with-make-json-object jansson* procedure))
        jansson-error*))))

;; returns whether a json object is of type object
(define (json-object-type-object? json-object)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-type (jansson-typeof jansson*)))
    (eq? jansson-type jansson-type-object)))

;; returns a property of a json object
(define (json-object-property json-object property-name)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-property* (jansson-object-get jansson* property-name)))
    (if jansson-property*
      (make-json-object jansson-property*)
      #f)))

;; returns whether a json object is of type array
(define (json-object-type-array? json-object)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-type (jansson-typeof jansson*)))
    (eq? jansson-type jansson-type-array)))

;; returns the json objects for the elements of an array
(define (json-object-array-elements json-object)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-type (jansson-typeof jansson*)))
    (if (eq? jansson-type jansson-type-array)
      (let ((jansson-array-size (jansson-array-size jansson*)))
        (map
          (lambda (index)
            (let ((jansson-element* (jansson-array-get jansson* index)))
              (make-json-object jansson-element*)))
          (iota jansson-array-size)))
      #f)))

;; returns whether a json object is of type value
(define (json-object-type-value? json-object)
  (not (or (json-object-type-object? json-object)
           (json-object-type-array? json-object))))

;; returns the value of a json object
(define (json-object-value json-object)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-type (jansson-typeof jansson*)))
    (cond ((eq? jansson-type jansson-type-string) (jansson-string-value jansson*))
          ((eq? jansson-type jansson-type-integer) (jansson-integer-value jansson*))
          ((eq? jansson-type jansson-type-real) (jansson-real-value jansson*))
          ((eq? jansson-type jansson-type-true) #t)
          (else #f))))

;; invokes a procedure with a new json object
(define (with-new-json-object procedure)
  (define (checked-jansson-object)
    (let ((jansson* (jansson-object)))
      (if (not jansson*)
        (abort "failed to create json object"))
        jansson*))
  (with-guaranteed-release
    checked-jansson-object
    (lambda (jansson*)
      (with-make-json-object jansson* procedure))
    jansson-decref))

;; invokes a procedure with a new json object representing a value
(define (with-new-json-object-from-value value procedure)
  (define (create-jansson*)
    (cond ((boolean? value) (jansson-boolean (if value 1 0)))
          ((and (integer? value) (exact? value)) (jansson-integer value))
          ((number? value) (jansson-real value))
          ((string? value) (jansson-string value))))
  (define (checked-create-jansson*)
    (let ((jansson* (create-jansson*)))
      (if (not jansson*)
        (abort "failed to create jansson object from value")
        jansson*)))
  (with-guaranteed-release
    checked-create-jansson*
    (lambda (jansson*)
      (with-make-json-object jansson* procedure))
    jansson-decref))

;; invokes a procedure with a new json object representing an array
(define (with-new-json-object-array procedure)
  (define (checked-jansson-array)
    (let ((jansson* (jansson-array)))
      (if (not jansson*)
        (abort "failed to create json object array")
        jansson*)))
  (with-guaranteed-release
    checked-jansson-array
    (lambda (jansson*)
      (with-make-json-object jansson* procedure))
    jansson-decref))

;; appends a json object to an array
(define (json-object-array-append! json-object json-object-element)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-element* (json-object-jansson* json-object-element))
         (jansson-array-append-result (jansson-array-append jansson* jansson-element*)))
    (when (not (eq? jansson-array-append-result 0))
      (abort "failed to append json object to array"))))

;; sets a property to a json object
(define (json-object-property-set! json-object property-name json-object-value)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-value* (json-object-jansson* json-object-value))
         (json-object-set-result (jansson-object-set jansson* property-name jansson-value*)))
    (when (not (eq? json-object-set-result 0))
      (abort "failed to set json object property"))))

;; serializes a json object to a string
(define (json-object->string json-object)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-dumps-result* (jansson-dumps-wrapped jansson*)))
    (when (not jansson-dumps-result*)
      (abort "failed to serialize json object"))
    (let ((jansson-dumps-result-value (jansson-dumps-result-value jansson-dumps-result*)))
      (jansson-free-dumps-result jansson-dumps-result*)
      jansson-dumps-result-value)))

;; returns whether a string represents an empty json object
(define (is-empty-json-object-string? string)
  (equal? string "{}"))
