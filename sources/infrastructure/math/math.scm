
(use srfi-1)

(declare (unit math))

;; returns the sum of the elements value
(define (math-sum
          elements
          element-value-procedure)

  (fold + 0 (map element-value-procedure elements)))

;; returns the average of the elements value
(define (math-average elements element-value-procedure)

  (let ((sum (math-sum elements element-value-procedure))
        (count (length elements)))

      (if (> count 0)
         (/ sum count)
         #f)))

;; adds a value to an average
(define (math-average-add-value
          previous-average previous-count
          value)

  (let* ((safe-previous-average (or previous-average 0))
         (previous-sum (* safe-previous-average previous-count))
         (new-sum (+ previous-sum value))
         (new-count (+ previous-count 1))
         (new-average (/ new-sum new-count)))

    new-average))

;; removes a value from an average
(define (math-average-remove-value
          previous-average
          previous-count value)

  (let* ((previous-sum (* previous-average previous-count))
         (new-sum (- previous-sum value))
         (new-count (- previous-count 1))
         (safe-new-average (if (= new-count 0) #f (/ new-sum new-count))))

    safe-new-average))
