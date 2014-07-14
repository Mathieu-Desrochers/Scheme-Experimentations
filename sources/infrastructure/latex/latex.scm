
(use files)
(use posix)
(use srfi-13)
(use utils)

(declare (unit latex))

(declare (uses exceptions))

;; returns the pdf compiled from a latex source
(define (latex-print-pdf latex-source)

  ;; create a temporary file
  (let ((temporary-file-path (create-temporary-file "tex")))

    ;; get the name of the other temporary files
    (let ((temporary-dvi-file-path (pathname-replace-extension temporary-file-path "dvi"))
          (temporary-pdf-file-path (pathname-replace-extension temporary-file-path "pdf"))
          (temporary-directory (pathname-directory temporary-file-path)))

      (with-guaranteed-release
        (lambda ()

          ;; open the temporary file
          (file-open
            temporary-file-path
            (+ open/wronly
               open/append
               open/creat)))

        (lambda (temporary-file-descriptor)

          ;; write the latex source to the temporary file
          (file-write temporary-file-descriptor latex-source)

          ;; invoke the latex process
          (process-wait
            (process-run
              "/usr/bin/latex"
              (list
                "-interaction=batchmode"
                (string-append "-output-directory=" temporary-directory)
                temporary-file-path)))

          ;; invoke the dvipdf process
          (process-wait
            (process-run
              "/usr/bin/dvipdf"
              (list
                temporary-dvi-file-path
                temporary-pdf-file-path)))

          ;; read the pdf file
          (string->blob
            (read-all
              temporary-pdf-file-path)))

        (lambda (temporary-file-descriptor)

          ;; close the temporary file
          (file-close temporary-file-descriptor))))))
