(define-module (gds data transformations build mysql)
  #:use-module (srfi srfi-1)
  #:export (decompress-file-and-pipe-to-mysql))

(define* (decompress-file-and-pipe-to-mysql file database)
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
            "mysql" ,(string-append "--database=" database))
          " ")))
    (simple-format #t "ungzip-file-and-pipe-to-mysql running:\n  ~A\n"
                   command)
    (force-output)
    (or (zero? (system command))
        (error "ungzip-file-and-pipe-to-mysql failed"))))
