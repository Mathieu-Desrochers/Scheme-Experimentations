
(declare (unit math))

;; adds a value to an average
(define (math-average-add-value previous-average previous-count value)
  (let* ((safe-previous-average (or previous-average 0))
         (previous-sum (* safe-previous-average previous-count))
         (new-sum (+ previous-sum value))
         (new-count (+ previous-count 1))
         (new-average (/ new-sum new-count)))
    new-average))

;; removes a value from an average
(define (math-average-remove-value previous-average previous-count value)
  (let* ((previous-sum (* previous-average previous-count))
         (new-sum (- previous-sum value))
         (new-count (- previous-count 1))
         (safe-new-average (if (= new-count 0) #f (/ new-sum new-count))))
    safe-new-average))
