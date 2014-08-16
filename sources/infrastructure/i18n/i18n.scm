
(use srfi-13)

(declare (unit i18n))

(declare (uses date-time))

;; returns whether a culture is known
(define (i18n-culture-known? culture)
  (or (equal? culture "en")
      (equal? culture "fr")))

;; localizes a string
(define (i18n-localize-string
          culture
          string-en
          string-fr)
  (cond ((equal? culture "en") string-en)
        ((equal? culture "fr") string-fr)))

;; localizes an amount of money
(define (i18n-localize-money culture number-of-cents)

  ;; localize the separators
  (let ((thousand-separator
          (cond ((equal? culture "en") ",")
                ((equal? culture "fr") " ")))
        (decimal-separator
          (cond ((equal? culture "en") ".")
                ((equal? culture "fr") ","))))

    ;; converts a number to a padded string
    (let ((number->padded-string
            (lambda (number padding-length)
              (string-append
                (make-string (- padding-length (string-length (number->string number))) #\0)
                (number->string number)))))

      ;; localizes a number of dollars
      (letrec ((i18n-localize-dollars-inner
                  (lambda (number-of-dollars accumulator)

                    ;; check if there are dollars left
                    (if (>= number-of-dollars 1)

                      ;; concatenate a new block of a thousand dollars
                      (i18n-localize-dollars-inner
                        (quotient number-of-dollars 1000)
                        (if (>= (quotient number-of-dollars 1000) 1)
                          (string-append
                            thousand-separator
                            (number->padded-string (remainder number-of-dollars 1000) 3)
                            accumulator)
                          (string-append
                            (number->string (remainder number-of-dollars 1000))
                            accumulator)))

                      ;; return the accumulated dollars
                      accumulator))))

        ;; localize the money amount
        (let ((localized-dollars (i18n-localize-dollars-inner (quotient number-of-cents 100) ""))
              (localized-cents (number->padded-string (remainder number-of-cents 100) 2)))

          ;; assemble the results
          (string-append
            (if (> (string-length localized-dollars) 0) localized-dollars "0")
            decimal-separator
            localized-cents
            " $"))))))

;; localizes a day-of-week
(define (i18n-day-of-week->string culture day-of-week)
  (let ((day-of-week-name (day-of-week-name day-of-week)))
    (cond
      ((equal? culture "en")
        (cond ((equal? day-of-week-name "Sunday") "Sunday")
              ((equal? day-of-week-name "Monday") "Monday")
              ((equal? day-of-week-name "Tuesday") "Tuesday")
              ((equal? day-of-week-name "Wednesday") "Wednesday")
              ((equal? day-of-week-name "Thursday") "Thursday")
              ((equal? day-of-week-name "Friday") "Friday")
              ((equal? day-of-week-name "Saturday") "Saturday")))
      ((equal? culture "fr")
        (cond ((equal? day-of-week-name "Sunday") "Dimanche")
              ((equal? day-of-week-name "Monday") "Lundi")
              ((equal? day-of-week-name "Tuesday") "Mardi")
              ((equal? day-of-week-name "Wednesday") "Mercredi")
              ((equal? day-of-week-name "Thursday") "Jeudi")
              ((equal? day-of-week-name "Friday") "Vendredi")
              ((equal? day-of-week-name "Saturday") "Samedi"))))))
