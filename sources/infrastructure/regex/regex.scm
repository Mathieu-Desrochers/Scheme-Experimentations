
(declare (unit regex))

(declare (uses regex-intern))

;; returns the pattern matches of a string
(define (regex-match pattern string)
  (with-regex pattern
    (lambda (pcre*)
      (with-regex-matches pcre* string
        (lambda (pcre-exec-result*)
          (let ((captures-count (pcre-exec-result-captures-count pcre-exec-result*)))
            (if (eq? captures-count 0)
              '()
              (map
                (lambda (capture-index)
                  (pcre-exec-result-capture pcre-exec-result* capture-index))
                (iota (- captures-count 1) 1)))))))))
