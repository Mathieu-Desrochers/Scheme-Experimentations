
(declare (unit exceptions))

;; hides any exception raised by a procedure
(define (hide-exceptions procedure)
  (handle-exceptions exception
    #f
    (procedure)))
