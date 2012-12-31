
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

;; returns the value of a property
(define (json-property-value json-object property-name)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-property* (jansson-object-get jansson* property-name)))
    (if jansson-property*
      (let ((jansson-property-type (jansson-typeof jansson-property*)))
        (cond ((eq? jansson-property-type jansson-object) #f)
              ((eq? jansson-property-type jansson-array) #f)
              ((eq? jansson-property-type jansson-string) (jansson-string-value jansson-property*))
              ((eq? jansson-property-type jansson-integer) (jansson-integer-value jansson-property*))
              ((eq? jansson-property-type jansson-real) (jansson-real-value jansson-property*))
              ((eq? jansson-property-type jansson-true) #t)
              ((eq? jansson-property-type jansson-false) #f)
              ((eq? jansson-property-type jansson-null) #f)))
      #f)))

;; invokes a procedure with the json object of a property
(define (json-property-object-value json-object property-name procedure)
  (let* ((jansson* (json-object-jansson* json-object))
         (jansson-property* (jansson-object-get jansson* property-name)))
    (if jansson-property*
      (let ((jansson-property-type (jansson-typeof jansson-property*)))
        (if (eq? jansson-property-type jansson-object)
          (let ((jansson-property-json-object (make-json-object jansson-property*)))
            (procedure jansson-property-json-object))
          #f))
      #f)))
