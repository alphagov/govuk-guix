(define-module (gds data transformations mongodb)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix monads)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages pv)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services databases)
  #:export (with-mongodb))

(define* (with-mongodb
          mongodb-service
          gexp-to-run
          #:key (base-directory #~(string-append
                                   (getcwd)
                                   "/mongodb")))
  (let ((mongodb (mongodb-configuration-mongodb
                  (service-parameters mongodb-service))))
    (with-imported-modules '((guix build utils))
      #~(let* ((pid-file "/tmp/mongodb.pid")
               (base-directory #$base-directory))

          (define (start-mongodb)
            ((make-forkexec-constructor
              (list #$(file-append mongodb "/bin/mongod")
                    (string-append "--pidfilepath=" pid-file)
                    "--dbpath" base-directory)
              #:pid-file "/tmp/mongodb.pid")))

          (add-to-load-path #$(file-append shepherd
                                           "/share/guile/site/"
                                           (effective-version)))

          (use-modules (shepherd service)
                       (guix build utils))

          (setenv "PATH"
                  (list->search-path-as-string
                   (cons*
                    #$(file-append mongo-tools "/bin")
                    #$(file-append pv "/bin")
                    #$(file-append gzip "/bin")
                    #$(file-append xz "/bin")
                    (search-path-as-string->list (getenv "PATH")))
                   ":"))

          (simple-format #t "activating Mongodb\n")
          (force-output)
          (mkdir-p base-directory)
          (chmod base-directory #o700)

          (let ((mongodb-pid (start-mongodb))
                (result (#$gexp-to-run '())))

            (simple-format #t "Stopping MongoDB\n")
            ((make-kill-destructor) mongodb-pid)
            ;; TODO: Fix this
            (sleep 20)
            (force-output)

            result)))))
