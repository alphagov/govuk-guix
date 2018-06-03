(define-module (gds data transformations build mysql)
  #:export (ungzip-file-and-pipe-to-mysql))

(define* (ungzip-file-and-pipe-to-mysql file database)
  (let ((command
         (string-join
          `("set -eo pipefail;"
            "pv" "--force" ,file "|"
            "gzip" "-d" "|"
            "mysql" ,(string-append "--database=" database))
          " ")))
    (simple-format #t "ungzip-file-and-pipe-to-mysql running:\n  ~A\n"
                   command)
    (force-output)
    (or (zero? (system command))
        (error "ungzip-file-and-pipe-to-mysql failed"))))
