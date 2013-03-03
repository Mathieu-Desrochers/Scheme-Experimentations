
(declare (unit json))

(declare (uses jansson))

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
    (cond ((eq? jansson-type jansson-type-object) #f)
          ((eq? jansson-type jansson-type-array) #f)
          ((eq? jansson-type jansson-type-string) (jansson-string-value jansson*))
          ((eq? jansson-type jansson-type-integer) (jansson-integer-value jansson*))
          ((eq? jansson-type jansson-type-real) (jansson-real-value jansson*))
          ((eq? jansson-type jansson-type-true) #t)
          ((eq? jansson-type jansson-type-false) #f)
          ((eq? jansson-type jansson-type-null) #f))))

;; returns the json object of a property
(define (json-object-property json-object property-name)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-property* (jansson-object-get jansson* property-name)))
    (if jansson-property*
      (make-json-object jansson-property*)
      #f)))

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

;; invokes a procedure with a new json object
(define (with-new-json-object procedure)
  (let ((jansson* (jansson-object)))
    (when (not jansson*)
      (abort "could not create new json object"))
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
      (abort "could not create new json object from value"))
    (handle-exceptions exception
      (begin
        (jansson-decref jansson*)
        (abort exception))
      (let* ((json-object (make-json-object jansson*))
             (procedure-result (procedure json-object)))
        (jansson-decref jansson*)
        procedure-result))))

;; invokes a procedure with a new json object representing an array
(define (with-new-json-object-array procedure)
  (let ((jansson* (jansson-array)))
    (when (not jansson*)
      (abort "could not create new json object array"))
    (handle-exceptions exception
      (begin
        (jansson-decref jansson*)
        (abort exception))
      (let* ((json-object (make-json-object jansson*))
             (procedure-result (procedure json-object)))
        (jansson-decref jansson*)
        procedure-result))))

;; appends a json object to an array
(define (json-object-array-append! json-object json-object-element)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-element* (json-object-jansson* json-object-element))
         (jansson-array-append-result (jansson-array-append jansson* jansson-element*)))
    (when (not (eq? jansson-array-append-result 0))
      (abort "could not append json object to array"))))

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
    (let ((jansson-dumps-result-value (jansson-dumps-result-value jansson-dumps-result*)))
      (jansson-free-dumps-result jansson-dumps-result*)
      jansson-dumps-result-value)))
