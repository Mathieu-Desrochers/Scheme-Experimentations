
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definition

(define-syntax define-test
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((test-symbol (list-ref exp 1))
             (test-steps (drop exp 2)))
        `(begin

          (declare (uses test))

          ;; define the test procedure
          (define (,test-symbol)
            (test-run
              ,(symbol->string test-symbol)

              ;; make the test steps
              (list
                ,@(map
                  (lambda (test-step)
                    `(make-test-step
                       ,(car test-step)
                       ,(cadr test-step)
                       ,(caddr test-step)))
                  test-steps)))))))))
