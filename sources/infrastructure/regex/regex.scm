
(use srfi-1)

(declare (unit regex))

(declare (uses regex-intern))

;; matches a string against a regular expression pattern
(define (regex-match pattern string)
  (with-regex pattern
    (lambda (pcre*)
      (with-regex-matches pcre* string
        (lambda (pcre-exec-result*)
          (let ((captures-count (pcre-exec-result-captures-count pcre-exec-result*)))
            (map
              (lambda (capture-index)
                (pcre-exec-result-capture pcre-exec-result* capture-index))
              (iota captures-count))))))))
