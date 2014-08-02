
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
