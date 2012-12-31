
(declare (unit jansson-ffi))

(foreign-declare "

#include <jansson.h>

// allocates a json_error on the heap
json_error_t* malloc_json_error()
{
  json_error_t* json_error = malloc(sizeof(json_error_t));
  return json_error;
}

// frees the specified json_error
void free_json_error(json_error_t* json_error)
{
  free(json_error);
}

")

;; json pointers definitions
(define-foreign-type jansson "json_t")
(define-foreign-type jansson* (c-pointer jansson))

;; json-error pointers definitions
(define-foreign-type jansson-error "json_error_t")
(define-foreign-type jansson-error* (c-pointer jansson-error))

;; json-error pointers memory management
(define malloc-jansson-error (foreign-lambda jansson-error* "malloc_json_error"))
(define free-jansson-error (foreign-lambda void "free_json_error" jansson-error*))

;; decodes the json string input
(define jansson-loads (foreign-lambda jansson* "json_loads" c-string unsigned-integer jansson-error*))

;; gets a value corresponding to key from object
(define jansson-object-get (foreign-lambda jansson* "json_object_get" jansson* c-string))

;; returns the type of the json value
(define jansson-typeof (foreign-lambda int "json_typeof" jansson*))

;; returns the associated value of integer
(define jansson-integer-value (foreign-lambda int "json_integer_value" jansson*))

;; returns the associated value of real
(define jansson-real-value (foreign-lambda double "json_real_value" jansson*))

;; returns the associated value of string
(define jansson-string-value (foreign-lambda c-string "json_string_value" jansson*))

;; returns the number of elements in array
(define jansson-array-size (foreign-lambda int "json_array_size" jansson*))

;; returns the element in array at position index
(define jansson-array-get (foreign-lambda jansson* "json_array_get" jansson* int))

;; decrements the reference count
(define jansson-decref (foreign-lambda void "json_decref" jansson*))

;; json types
(define jansson-object (foreign-value "JSON_OBJECT" int))
(define jansson-array (foreign-value "JSON_ARRAY" int))
(define jansson-string (foreign-value "JSON_STRING" int))
(define jansson-integer (foreign-value "JSON_INTEGER" int))
(define jansson-real (foreign-value "JSON_REAL" int))
(define jansson-true (foreign-value "JSON_TRUE" int))
(define jansson-false (foreign-value "JSON_FALSE" int))
(define jansson-null (foreign-value "JSON_NULL" int))
