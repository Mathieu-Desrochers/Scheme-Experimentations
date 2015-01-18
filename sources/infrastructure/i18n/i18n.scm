
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

;; localizes a date month
(define (i18n-date->string-month culture date)
  (let ((date-month (date-month date)))
    (cond
      ((equal? culture "en")
        (cond ((equal? date-month 1) "January")
              ((equal? date-month 2) "February")
              ((equal? date-month 3) "March")
              ((equal? date-month 4) "April")
              ((equal? date-month 5) "May")
              ((equal? date-month 6) "June")
              ((equal? date-month 7) "July")
              ((equal? date-month 8) "August")
              ((equal? date-month 9) "September")
              ((equal? date-month 10) "October")
              ((equal? date-month 11) "November")
              ((equal? date-month 12) "December")))
      ((equal? culture "fr")
        (cond ((equal? date-month 1) "janvier")
              ((equal? date-month 2) "février")
              ((equal? date-month 3) "mars")
              ((equal? date-month 4) "avril")
              ((equal? date-month 5) "mai")
              ((equal? date-month 6) "juin")
              ((equal? date-month 7) "juillet")
              ((equal? date-month 8) "août")
              ((equal? date-month 9) "septembre")
              ((equal? date-month 10) "octobre")
              ((equal? date-month 11) "novembre")
              ((equal? date-month 12) "décembre"))))))

;; localizes a date using the long format
(define (i18n-date->string-long-format culture date)
  (cond
    ((equal? culture "en")
      (string-append
        (i18n-date->string-month culture date)
        " "
        (number->string (date-day date))
        ", "
        (number->string (date-year date))))
    ((equal? culture "fr")
      (string-append
        (number->string (date-day date))
        " "
        (i18n-date->string-month culture date)
        " "
        (number->string (date-year date))))))
