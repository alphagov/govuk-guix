(define-module (gds scripts govuk system passphrase)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-26)
  #:use-module (guix ui)
  #:export (passphrase))

(define (passphrase opts)
  (or (and=> (getenv "GOVUK_GUIX_DEVELOPMENT_PASSPHRASE")
             (lambda (passphrase)
               (simple-format
                #t
                "The passphrase (set through GOVUK_GUIX_DEVELOPMENT_PASSPHRASE) is: ~A"
                passphrase)))
      (let ((data-dir (or (getenv "XDG_DATA_HOME")
                          (and=> (getenv "HOME")
                                 (cut string-append <> "/.local/share")))))
        (if (file-exists? data-dir)
            (let* ((passphrase-file
                    (string-append
                     data-dir
                     "/govuk-guix/systems/development/passphrase")))
              (if (file-exists? passphrase-file)
                  (let ((passphrase
                         (call-with-input-file passphrase-file read-line)))
                    (simple-format
                     #t
                     "The passphrase for the development system is recorded in ~A. It is:\n\n  ~A\n\n"
                     passphrase-file
                     passphrase))
                  (leave
                   (G_ "The passphrase file could not be found at ~A")
                   passphrase-file)))
            (leave
             (G_ "The data directory could not be determined"))))))
