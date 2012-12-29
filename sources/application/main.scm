
(declare (uses jansson-ffi))

(let ((json-error* (malloc-json-error)))
  (let ((json* (json-loads "{\"name\":\"Mathieu\"")))
    (display "yeah"))
  (free-json-error json-error*))
