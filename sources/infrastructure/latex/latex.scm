
(use files)
(use posix)
(use srfi-13)
(use utils)

(declare (unit latex))

(declare (uses exceptions))

;; formats a number to a latex string that uses vulgar fraction instructions
(define (latex-number->string-with-vulgar-fraction number)
  (let* ((full-units (inexact->exact (floor number)))
         (partial-units (- number full-units)))
    (string-append
      (number->string full-units)
      (cond ((>= partial-units 0.75) "\\nicefrac{3}{4}")
            ((>= partial-units 0.5) "\\nicefrac{1}{2}")
            ((>= partial-units 0.25) "\\nicefrac{1}{4}")
            (else "")))))

;; returns the pdf compiled from a latex source
(define (latex-print-pdf latex-source)

  ;; get the temporary file paths
  (let* ((temporary-tex-file-path (create-temporary-file "tex"))
         (temporary-pdf-file-path (pathname-replace-extension temporary-tex-file-path "pdf"))
         (temporary-directory (pathname-directory temporary-tex-file-path)))

    (with-guaranteed-release
      (lambda ()

        ;; open the temporary latex file
        (file-open
          temporary-tex-file-path
          (+ open/wronly
             open/append
             open/creat)))

      (lambda (temporary-file-descriptor)

        ;; write the latex source to the temporary file
        (file-write temporary-file-descriptor latex-source)

        ;; invoke the latex process
        (process-wait
          (process-run
            "/usr/bin/pdflatex"
            (list
              "-interaction=batchmode"
              (string-append "-output-directory=" temporary-directory)
              temporary-tex-file-path)))

        ;; read the temporary pdf file
        (string->blob
          (read-all
            temporary-pdf-file-path)))

      (lambda (temporary-file-descriptor)

        ;; close the temporary file
        (file-close temporary-file-descriptor)))))
