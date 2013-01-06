
(require-extension utf8)

(use srfi-1)

(declare (uses json))

(with-new-json-object
  (lambda (json-object)
    (with-new-json-object-from-value "Mathieu"
      (lambda (json-object-from-value)
        (json-object-property-set! json-object "name" json-object-from-value)
        (display (json-object->string json-object))))))
