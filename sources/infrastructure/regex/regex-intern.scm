
(declare (unit regex-intern))

(declare (uses exceptions))
(declare (uses pcre))

;; invokes a procedure with a regular expression
(define (with-regex pattern procedure)
  (define (checked-pcre-compile)
    (let ((pcre* (pcre-compile pattern)))
      (if (not pcre*)
        (abort "failed to compile regular expression")
        pcre*)))
  (with-guaranteed-release
    checked-pcre-compile
    procedure
    free-pcre))

;; invokes a procedure with the matches of a regular expression
(define (with-regex-matches pcre* string procedure)
  (define (checked-pcre-exec)
    (let ((pcre-exec-result* (pcre-exec pcre* string)))
      (if (not pcre-exec-result*)
        (abort "failed to execute regular expression")
        pcre-exec-result*)))
  (with-guaranteed-release
    checked-pcre-exec
    procedure
    free-pcre-exec-result))
