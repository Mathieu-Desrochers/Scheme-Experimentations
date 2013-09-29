
(declare (unit records))

;; increments a record value
(define (increment record get-procedure set-procedure by)
  (let* ((current-value (get-procedure record))
         (new-value (+ current-value by)))
    (set-procedure record new-value)))
