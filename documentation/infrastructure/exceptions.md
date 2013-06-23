
with-exception-hidding
----------------------
Invokes a procedure and hides any exception it raises.

__invocation__

    (with-exception-hidding
      (lambda ()
        (display "being nasty\n")
        (/ 1 0)))

    (display "still running\n")

__output__

    being nasty
    still running

with-guaranteed-release
-----------------------
Makes sure a resource is released even in the face of an exception.

__allocation-procedure__

Procedure that allocates and returns a resource.

__procedure__

Procedure invoked with the allocated resource.  
Can perform work without worrying about its release.

__release-procedure__

Procedure invoked with the allocated resource.  
Responsible for its release.

__invocation__

    (with-guaranteed-release
      (lambda () "expensive resource")
      (lambda (resource) (/ 1 0))
      (lambda (resource) (display (string-append "releasing " resource))))

__output__

    releasing expensive resource
