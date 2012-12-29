
(declare (uses jansson-ffi))

(let* ((json-error* (malloc-json-error))
       (json-root* (json-loads "{\"name\":\"Mathieu\"}" 0 json-error*))
       (json-name* (json-object-get json-root* "name")))
  (display (json-string-value json-name*))
  (json-decref json-root*)
  (free-json-error json-error*))
