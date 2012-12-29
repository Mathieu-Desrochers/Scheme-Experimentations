
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
(define-foreign-type json "json_t")
(define-foreign-type json* (c-pointer json))

;; json-error pointers definitions
(define-foreign-type json-error "json_error_t")
(define-foreign-type json-error* (c-pointer json-error))

;; json-error pointers memory management
(define malloc-json-error (foreign-lambda json-error* "malloc_json_error"))
(define free-json-error (foreign-lambda void "free_json_error" json-error*))

;; decodes the json string input
(define json-loads (foreign-lambda json* "json_loads" c-string unsigned-integer json-error*))

;; get a value corresponding to key from object
(define json-object-get (foreign-lambda json* "json_object_get" json* c-string))

;; returns the associated value of string
(define json-string-value (foreign-lambda c-string "json_string_value" json*))

;; decrements the reference count
(define json-decref (foreign-lambda void "json_decref" json*))
