
(declare (unit regex-intern))

(declare (uses pcre))

;; invokes a procedure with a regular expression
(define (with-regex pattern procedure)
  (let ((pcre* (pcre-compile pattern)))
    (when (not pcre*)
      (abort "could not compile regular expression"))
    (handle-exceptions exception
      (begin
        (free-pcre pcre*)
        (abort exception))
      (let ((procedure-result (procedure pcre*)))
        (free-pcre pcre*)
        procedure-result))))

;; invokes a procedure with the matches of a regular expression
(define (with-regex-matches pcre* string procedure)
  (let ((pcre-exec-result* (pcre-exec pcre* string)))
    (when (not pcre-exec-result*)
      (abort "could not execute regular expression"))
    (handle-exceptions exception
      (begin
        (free-pcre-exec-result pcre-exec-result*)
        (abort exception))
      (let ((procedure-result (procedure pcre-exec-result*)))
        (free-pcre-exec-result pcre-exec-result*)
        procedure-result))))
