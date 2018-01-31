(define-module (gds services govuk publishing-e2e-tests)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (gds packages govuk)
  #:use-module (gds services utils)
  #:use-module (gds services govuk plek)
  #:use-module (gds services govuk signon)
  #:export (publishing-e2e-tests-signon-users
            publishing-e2e-tests-environment-variables

            publishing-e2e-tests-service-type))

(define publishing-e2e-tests-signon-user-count 5)

(define publishing-e2e-tests-results-directory
  "/var/apps/publishing-e2e-tests/tmp/results")

(define publishing-e2e-tests-signon-users
  (map (lambda (number)
         (signon-user
          (name (simple-format #f "User ~s" number))
          (email (simple-format #f "user-~s@dev.gov.uk" number))
          (passphrase (random-base16-string 16))
          (role (if (eq? number 0)
                    "superadmin"
                    "normal"))))
       (iota publishing-e2e-tests-signon-user-count)))

(define (publishing-e2e-tests-environment-variables plek-config signon-users)
  (append
   `(("SIGNON_USER_COUNT" . ,(number->string (length signon-users)))
     ("CAPYBARA_SAVE_PATH" . "/var/apps/publishing-e2e-tests/tmp/results")
     ("GOVUK_CONTENT_SCHEMAS_PATH" . "/var/apps/govuk-content-schemas")
     ("SSL_CERT_FILE" .
      "/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"))
   (plek-config->environment-variables plek-config)
   (append-map (lambda (user number)
                 `((,(simple-format #f "SIGNON_USER_~s_EMAIL" number)
                    .
                    ,(signon-user-email user))
                   (,(simple-format #f "SIGNON_USER_~s_PASSPHRASE" number)
                    .
                    ,(signon-user-passphrase user))))
               publishing-e2e-tests-signon-users
               (iota (length signon-users)))))

(define publishing-e2e-tests-program-file
  (program-file
   "start-publishing-e2e-tests"
   (with-imported-modules (source-module-closure
                           '((guix build utils)))
     #~(let* ((results-directory #$publishing-e2e-tests-results-directory)
              (test-results.html
               (string-append results-directory "/test-results.html"))
              (result
               (zero? (system*
                       "/var/apps/publishing-e2e-tests/bin/bundle"
                       "exec"
                       "rspec"
                       "--tag" "~flaky" "--tag" "~new"
                       "--format" "documentation"
                       "--format" "html"
                       "--out" test-results.html))))

         (use-modules (guix build utils)
                      (ice-9 rdelim)
                      (ice-9 rw))

         ;; Links to pages and screenshots are absolute, so turn
         ;; them in to relative links so that they work outside of
         ;; the container
         (let ((substring
                (string-append "file://" results-directory "/")))
           (with-atomic-file-replacement
            test-results.html
            (lambda (in out)
              (write-string/partial
               (let filter ((str (read-string in)))
                 (let ((index (string-contains str substring)))
                   (if index
                       (filter
                        (string-replace
                         str
                         ""
                         index
                         (+ index (string-length substring))))
                       str)))
               out))))

         (if result
             (call-with-output-file
                 (string-append results-directory "/all-tests-succeeded")
               (lambda (port)
                 (simple-format port ""))))

         result))))

(define publishing-e2e-tests-activation-gexp
  (match-lambda
    ((plek-config package)
     (with-imported-modules (source-module-closure
                             '((guix build syscalls)
                               (gnu build file-systems)
                               (guix build utils)
                               (gnu services herd)))
       #~(let* ((results-directory #$publishing-e2e-tests-results-directory)
                (test-results.html
                 (string-append results-directory "/test-results.html")))

           (use-modules (guix build utils)
                        (gnu services herd)
                        (gnu build file-systems)
                        (guix build syscalls))

           (mkdir-p results-directory)
           (if (file-exists? "/var/apps/publishing-e2e-tests/bin")
               (begin
                 (mount "tmpfs" "/var/apps/publishing-e2e-tests/bin" "tmpfs")
                 (copy-recursively
                  (string-append #$package "/bin")
                  "/var/apps/publishing-e2e-tests/bin"
                  #:log (%make-void-port "w")
                  #:follow-symlinks? #f)
                 (for-each delete-file
                           (find-files "/var/apps/publishing-e2e-tests/tmp"
                                       "user.*\\_two\\_step\\_verification\\_secret")))
               (begin
                 (mkdir-p "/var/apps/publishing-e2e-tests")
                 (copy-recursively
                  #$package
                  "/var/apps/publishing-e2e-tests"
                  #:log (%make-void-port "w")
                  #:follow-symlinks? #f)))

           (chmod results-directory #o777)
           (chmod "/var/apps/publishing-e2e-tests/tmp" #o777)

           (let* ((dir (string-append "/tmp/env.d/"))
                  (file (string-append dir "publishing-e2e-tests")))
             (mkdir-p dir)
             (call-with-output-file file
               (lambda (port)
                 (for-each
                  (lambda (env-var)
                    (simple-format port "export ~A=\"~A\"\n" (car env-var) (cdr env-var)))
                  '#$(publishing-e2e-tests-environment-variables
                      plek-config
                      publishing-e2e-tests-signon-users))))))))))

(define publishing-e2e-tests-service-shepherd-services
  (match-lambda
    ((plek-config package)
     (list
      (shepherd-service
       (provision (list 'publishing-e2e-tests))
       (documentation "publishing-e2e-tests")
       (requirement '(specialist-publisher
                      government-frontend
                      draft-government-frontend
                      contacts-admin
                      finder-frontend
                      content-tagger
                      whitehall
                      travel-advice-publisher
                      collections-publisher
                      collections
                      publisher
                      manuals-publisher
                      frontend
                      nginx
                      asset-manager))
       (respawn? #f)
       (start #~(lambda args
                  (waitpid
                   (fork+exec-command
                    '(#$publishing-e2e-tests-program-file)
                    #:user "publishing-e2e-tests"
                    #:directory "/var/apps/publishing-e2e-tests"
                    #:environment-variables
                    '#$(map
                        (lambda (env-var)
                          (simple-format #f "~A=~A" (car env-var) (cdr env-var)))
                        (append
                         (plek-config->environment-variables plek-config)
                         (publishing-e2e-tests-environment-variables
                          plek-config
                          publishing-e2e-tests-signon-users)))))

                  ;; Change file permissions to be writable by all
                  (for-each
                   (lambda (f) (chmod f #o666))
                   (find-files #$publishing-e2e-tests-results-directory))

                  (fork+exec-command
                   '(#$(program-file
                        "stop-root"
                        (with-imported-modules (source-module-closure
                                                '((gnu services herd)))
                          #~(begin
                              (sleep 1)
                              (use-modules (gnu services herd))
                              (stop-service 'root))))))

                  #t))
       (stop #~(make-kill-destructor)))))))

(define publishing-e2e-tests-service-type
  (service-type
   (name 'publishing-e2e-tests)
   (extensions
    (list (service-extension signon-service-type
                             (const publishing-e2e-tests-signon-users))
          (service-extension account-service-type
                             (const
                              (list
                               (user-account
                                (name "publishing-e2e-tests")
                                (group "nogroup")
                                (system? #t)
                                (home-directory "/var/apps/publishing-e2e-tests")
                                (create-home-directory? #f)
                                (shell #~(string-append #$shadow "/sbin/nologin"))))))
          (service-extension shepherd-root-service-type
                             publishing-e2e-tests-service-shepherd-services)
          (service-extension activation-service-type
                             publishing-e2e-tests-activation-gexp)))
   (default-value
     (list (plek-config) publishing-e2e-tests))))
