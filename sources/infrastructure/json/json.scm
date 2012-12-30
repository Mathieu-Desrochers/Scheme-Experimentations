
(declare (unit json))
(declare (uses jansson-ffi))

;; encapsulates a json object
(define-record json-object jansson*)

;; gets the value of a property
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
