
(declare (uses jansson-ffi))
(declare (uses json))

(let* ((jansson-error* (malloc-jansson-error))
       (json-object (make-json-object (jansson-loads "{\"name\":\"value\"}" 0 jansson-error*)))
       (value (json-property-value json-object "name")))
  (display value)
  (jansson-decref (json-object-jansson* json-object))
  (free-jansson-error jansson-error*))
