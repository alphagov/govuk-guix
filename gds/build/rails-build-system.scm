(define-module (gds build rails-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (sxml simple)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            rails-build

            wrap-ruby-program))

;; Commentary:
;;
;; Builder-side code of the standard build procedure for applications
;; using Rails.
;;
;; Code:

(define* (wrap-ruby-program prog #:key (gem-clear-paths #t) #:rest vars)
  "Make a wrapper for PROG.  VARS should look like this:

  '(VARIABLE DELIMITER POSITION LIST-OF-DIRECTORIES)

where DELIMITER is optional.  ':' will be used if DELIMITER is not given.

For example, this command:

  (wrap-ruby-program \"foo\"
                '(\"PATH\" \":\" = (\"/gnu/.../bar/bin\"))
                '(\"CERT_PATH\" suffix (\"/gnu/.../baz/certs\"
                                        \"/qux/certs\")))

will copy 'foo' to '.real/fool' and create the file 'foo' with the following
contents:

  #!location/of/bin/ruby
  ENV['PATH'] = \"/gnu/.../bar/bin\"
  ENV['CERT_PATH'] = (ENV.key?('CERT_PATH') ? (ENV['CERT_PATH'] + ':') : '') + '/gnu/.../baz/certs:/qux/certs'
  load location/of/.real/foo

This is useful for scripts that expect particular programs to be in $PATH, for
programs that expect particular gems to be in the GEM_PATH.

This is preferable to wrap-program, which uses a bash script, as this prevents
ruby scripts from being executed with @command{ruby -S ...}.

If PROG has previously been wrapped by 'wrap-ruby-program', the wrapper is
extended with definitions for VARS."
  (define wrapped-file
    (string-append (dirname prog) "/.real/" (basename prog)))

  (define already-wrapped?
    (file-exists? wrapped-file))

  (define (last-line port)
    ;; Return the last line read from PORT and leave PORT's cursor right
    ;; before it.
    (let loop ((previous-line-offset 0)
               (previous-line "")
               (position (seek port 0 SEEK_CUR)))
      (match (read-line port 'concat)
        ((? eof-object?)
         (seek port previous-line-offset SEEK_SET)
         previous-line)
        ((? string? line)
         (loop position line (+ (string-length line) position))))))

  (define (export-variable lst)
    ;; Return a string that exports an environment variable.
    (match lst
      ((var sep '= rest)
       (format #f "ENV['~a'] = '~a'"
               var (string-join rest sep)))
      ((var sep 'prefix rest)
       (format #f "ENV['~a'] = '~a' + (ENV.key?('~a') ? ('~a' + ENV['~a']) : '')"
               var (string-join rest sep) var sep var))
      ((var sep 'suffix rest)
       (format #f "ENV['~a'] = (ENV.key?('~a') ? (ENV['~a'] + '~a') : '') + '~a'"
               var var var sep (string-join rest sep)))
      ((var '= rest)
       (format #f "ENV['~a'] = '~a'"
               var (string-join rest ":")))
      ((var 'prefix rest)
       (format #f "ENV['~a'] = '~a' + (ENV.key?('~a') ? (':' + ENV['~a']) : '')"
               var (string-join rest ":") var var))
      ((var 'suffix rest)
       (format #f "ENV['~a'] = (ENV.key?('~a') ? (ENV['~a'] + ':') : '') + '~a'"
               var var var (string-join rest ":")))))

  (if already-wrapped?

      ;; PROG is already a wrapper: add the new "export VAR=VALUE" lines just
      ;; before the last line.
      (let* ((port (open-file prog "r+"))
             (last (last-line port)))
        (for-each (lambda (var)
                    (display (export-variable var) port)
                    (newline port))
                  vars)
        (display last port)
        (close-port port))

      ;; PROG is not wrapped yet: create a shell script that sets VARS.
      (let ((prog-tmp (string-append wrapped-file "-tmp")))
        (mkdir-p (dirname prog-tmp))
        (link prog wrapped-file)

        (call-with-output-file prog-tmp
          (lambda (port)
            (format port
                    "#!~a~%~a~%~a~%load '~a'~%"
                    (which "ruby")
                    (string-join (map export-variable vars) "\n")
                    ;; This ensures that if the GEM_PATH has been changed,
                    ;; then that change will be noticed.
                    (if gem-clear-paths "Gem.clear_paths" "")
                    (canonicalize-path wrapped-file))))

        (chmod prog-tmp #o755)
        (rename-file prog-tmp prog))))

(define* (replace-relative-spring-path #:key outputs
                                       #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (files
          (find-files
           (string-append out "/bin")
           (lambda (name stat)
             (or
              (access? name X_OK)
              (begin
                (simple-format
                 #t
                 "Skipping wrapping ~A as its not executable\n" name)
                #f))))))
    (substitute* files
      (("File\\.expand_path\\([\"']\\.\\./spring[\"'], __FILE__\\)")
       "File.expand_path('../.spring-real', __FILE__)")))
  #t)

(define* (wrap-with-relative-path #:key outputs
                                  #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (files
          (find-files
           (string-append out "/bin")
           (lambda (name stat)
             (or
              (access? name X_OK)
              (begin
                (simple-format
                 #t
                 "Skipping wrapping ~A as its not executable\n" name)
                #f))))))
    (substitute* files
      (((string-append out "/bin"))
       "${BASH_SOURCE%/*}")))
  #t)

(define* (create-tmp-directory #:key outputs
                              #:allow-other-keys)
  (mkdir-p (string-append
            (assoc-ref outputs "out")
            "/tmp")))

(define* (create-log-directory #:key outputs #:allow-other-keys)
  (mkdir-p (string-append
            (assoc-ref outputs "out")
            "/log")))

(define* (precompile-rails-assets
          #:key inputs precompile-rails-assets?
          #:allow-other-keys)
  (or (not precompile-rails-assets?)
      (if (file-exists? "bin/bundle")
          (invoke "bundle" "exec" "rake" "assets:precompile")
          (invoke "rails" "assets:precompile"))))

(define* (install #:key inputs outputs exclude-files #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (install-file?
          (negate (lambda (f)
                    (member f
                            (append exclude-files
                                    '("." ".."))))))
         (files (scandir "." install-file?)))

    (simple-format #t "exclude-files: ~A\n" exclude-files)
    (mkdir-p out)
    (for-each (lambda (file)
                (if (directory-exists? file)
                    (copy-recursively
                     file
                     (string-append out "/" file)
                     #:log (%make-void-port "w"))
                    (copy-file file (string-append out "/" file))))
              files))
  #t)

(define* (wrap-bin-files-for-rails #:key inputs outputs #:allow-other-keys)
  (for-each
   (lambda (script)
     (wrap-program
         script
       `("PATH" ":" prefix (,(string-append
                              (assoc-ref inputs "node")
                              "/bin")))))
   (find-files
    (string-append (assoc-ref outputs "out") "/bin")
    (lambda (name stat)
      (and
       (not (string-prefix? "." (last (string-split name #\/))))
       (or
        (access? name X_OK)
        (begin
          (simple-format #t "Skipping wrapping ~A as its not executable\n" name)
          #f))))))
  #t)

(define* (patch-bin-files #:key inputs outputs #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out")))
    (substitute*
        (find-files
         (string-append out "/bin")
         (lambda (name stat)
           (or
            (access? name X_OK)
            (begin
              (simple-format #t "Skipping patching ~A as its not executable\n" name)
              #f))))
      (("/usr/bin/env") (which "env"))))
  #t)

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'configure (lambda args #t))
    (replace 'build (lambda args #t))
    (replace 'check (lambda args #t))
    (replace 'install install)
    (add-before 'install 'precompile-rails-assets
                precompile-rails-assets)
    (add-after 'install 'wrap-bin-files-for-rails
               wrap-bin-files-for-rails)
    (add-after 'wrap-bin-files-for-rails 'replace-relative-spring-path
               replace-relative-spring-path)
    (add-after 'install 'create-tmp-directory
               create-tmp-directory)
    (add-after 'create-tmp-directory 'create-log-directory
               create-log-directory)
    (add-after 'create-log-directory 'patch-bin-files
               patch-bin-files)
    (add-after 'patch-bin-files 'wrap-with-relative-path
               wrap-with-relative-path)))

(define* (rails-build #:key
                      inputs
                      (phases %standard-phases)
                      #:allow-other-keys
                      #:rest args)
  "Build the given Rails application, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
