
(declare (unit exceptions))

;; hides any exception raised by a procedure
(define (with-exception-hidding procedure)
  (handle-exceptions exception
    #f
    (procedure)))

;; invokes a procedure with the guarantee
;; the allocated resource will be released
(define (with-guaranteed-release allocation-procedure procedure release-procedure)
  (let ((original-exception-handler (current-exception-handler))
        (allocated-resource (allocation-procedure)))
    (handle-exceptions exception
      (begin
        (release-procedure allocated-resource)
        (original-exception-handler exception))
      (let ((procedure-result (procedure allocated-resource)))
        (release-procedure allocated-resource)
        procedure-result))))
