
(use files)
(use posix)
(use srfi-13)
(use utils)

(declare (unit latex))

(declare (uses exceptions))

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
