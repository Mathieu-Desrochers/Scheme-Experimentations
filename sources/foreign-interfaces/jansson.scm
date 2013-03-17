
(declare (unit jansson))

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

// the result of the wrapped json_dumps function
struct json_dumps_result_t
{
  char* value;
};

// wraps the json_dumps function
// returns an opaque heap pointer to scheme
struct json_dumps_result_t* json_dumps_wrapped(json_t* json)
{
  char* value = json_dumps(json, JSON_INDENT(2) | JSON_PRESERVE_ORDER);
  if (value == NULL)
  {
    return NULL;
  }

  struct json_dumps_result_t* json_dumps_result = malloc(sizeof(struct json_dumps_result_t));
  if (json_dumps_result == NULL)
  {
    free(value);
    return NULL;
  }

  json_dumps_result->value = value;
  return json_dumps_result;
}

// returns the value of a json_dumps_result
// allows scheme to take a copy of the string
char* json_dumps_result_value(struct json_dumps_result_t* json_dumps_result)
{
  char* value = json_dumps_result->value;
  return value;
}

// frees the specified json_dumps_result
// as well as the value it points to
void json_free_dumps_result(struct json_dumps_result_t* json_dumps_result)
{
  free(json_dumps_result->value);
  free(json_dumps_result);
}

")

;; jansson pointers definitions
(define-foreign-type jansson "json_t")
(define-foreign-type jansson* (c-pointer jansson))

;; jansson-error pointers definitions
(define-foreign-type jansson-error "json_error_t")
(define-foreign-type jansson-error* (c-pointer jansson-error))

;; jansson-dumps-result pointers definitions
(define-foreign-type jansson-dumps-result "struct json_dumps_result_t")
(define-foreign-type jansson-dumps-result* (c-pointer jansson-dumps-result))

;; jansson-error pointers memory management
(define malloc-jansson-error (foreign-lambda jansson-error* "malloc_json_error"))
(define free-jansson-error (foreign-lambda void "free_json_error" jansson-error*))

;; jansson types
(define jansson-type-object (foreign-value "JSON_OBJECT" int))
(define jansson-type-array (foreign-value "JSON_ARRAY" int))
(define jansson-type-string (foreign-value "JSON_STRING" int))
(define jansson-type-integer (foreign-value "JSON_INTEGER" int))
(define jansson-type-real (foreign-value "JSON_REAL" int))
(define jansson-type-true (foreign-value "JSON_TRUE" int))
(define jansson-type-false (foreign-value "JSON_FALSE" int))
(define jansson-type-null (foreign-value "JSON_NULL" int))

;; decodes the jansson string input
(define jansson-loads (foreign-lambda jansson* "json_loads" c-string unsigned-integer jansson-error*))

;; gets a value corresponding to key from object
(define jansson-object-get (foreign-lambda jansson* "json_object_get" jansson* c-string))

;; returns the type of the jansson value
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

;; returns a new jansson object
(define jansson-object (foreign-lambda jansson* "json_object"))

;; sets the value of key to value in object
(define jansson-object-set (foreign-lambda int "json_object_set" jansson* c-string jansson*))

;; returns a new jansson boolean
(define jansson-boolean (foreign-lambda jansson* "json_boolean" int))

;; returns a new jansson integer
(define jansson-integer (foreign-lambda jansson* "json_integer" int))

;; returns a new jansson real
(define jansson-real (foreign-lambda jansson* "json_real" double))

;; returns a new jansson string
(define jansson-string (foreign-lambda jansson* "json_string" c-string))

;; returns a new jansson null
(define jansson-null (foreign-lambda jansson* "json_null"))

;; returns a new jansson array
(define jansson-array (foreign-lambda jansson* "json_array"))

;; appends to the end of array
(define jansson-array-append (foreign-lambda int "json_array_append" jansson* jansson*))

;; returns the jansson representation as a string
(define jansson-dumps-wrapped (foreign-lambda jansson-dumps-result* "json_dumps_wrapped" jansson*))
(define jansson-dumps-result-value (foreign-lambda c-string "json_dumps_result_value" jansson-dumps-result*))
(define jansson-free-dumps-result (foreign-lambda void "json_free_dumps_result" jansson-dumps-result*))

;; decrements the reference count
(define jansson-decref (foreign-lambda void "json_decref" jansson*))
