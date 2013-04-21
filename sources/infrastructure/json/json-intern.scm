
(declare (unit json-intern))

(declare (uses exceptions))

;; invokes a procedure with a jansson-error*
(define (with-jansson-error* procedure)
  (define (checked-malloc-jansson-error)
    (let ((jansson-error* (malloc-jansson-error)))
      (if (not jansson-error*)
        (abort "failed to allocate jansson-error"))
        jansson-error*))
  (with-guaranteed-release
    checked-malloc-jansson-error
    procedure
    free-jansson-error))

;; invokes a procedure with a loaded jansson*
(define (with-loaded-jansson* string procedure jansson-error*)
  (define (checked-jansson-loads)
    (let ((jansson* (jansson-loads string 0 jansson-error*)))
      (if (not jansson*)
        (abort "failed to load json string"))
        jansson*))
  (with-guaranteed-release
    checked-jansson-loads
    procedure
    jansson-decref))

;; invokes a procedure with a json-object
(define (with-make-json-object jansson* procedure)
  (let* ((json-object (make-json-object jansson*))
         (procedure-result (procedure json-object)))
    procedure-result))
