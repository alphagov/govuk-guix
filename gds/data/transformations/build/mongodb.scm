(define-module (gds data transformations build mongodb)
  #:use-module (srfi srfi-1)
  #:export (decompress-file-and-pipe-to-mongorestore))

(define* (decompress-file-and-pipe-to-mongorestore file database)
  (define decompressor
    (assoc-ref '(("gz" . "gzip")
                 ("xz" . "xz"))
               (last (string-split file #\.))))

  (let ((command
         (string-join
          `("set -eo pipefail;"
            "pv" "--force" ,file "|"
            ,@(if decompressor
                  `(,decompressor "-d" "|")
                  '())
            "mongorestore" "--quiet" "-d" ,database "--archive")
          " ")))
    (simple-format #t "ungzip-file-and-pipe-to-mongorestore running:\n  ~A\n"
                   command)
    (force-output)
    (or (zero? (system command))
        (error "ungzip-file-and-pipe-to-mongorestore failed"))))
