
(define-macro (let-optionals rest-arg bindings . body)
  (if (null? bindings)
`(let () ,@body)
(let ((proc (lambda (optional)
        `(,(car optional)
    (cond
     ((not (null? ,rest-arg))
      (let ((result (car ,rest-arg)))
        ,(list 'set! rest-arg
         `(cdr ,rest-arg))
        result))
     (else
      ,(cadr optional))))))
      (bindings (map (lambda (x)
           (if (list? x)
         x
         (list x #f)))
         bindings)))
  `(let ,(map proc bindings) ,@body))))

(define (iota count . maybe-start+step)
  (if (< count 0) (error "Negative step count" iota count))
  (let-optionals maybe-start+step ((start 0) (step 1))
    (let loop ((n 0) (r '()))
      (if (= n count)
    (reverse r)
    (loop (+ 1 n)
    (cons (+ start (* n step)) r))))))
