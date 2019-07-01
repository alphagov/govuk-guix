(define-module (gds build-jobs cuirass-entry-point)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (govuk-package-jobs))

(define working-directory
  (getcwd))

(define (govuk-packages-jobs . args)
  (let* ((port (open-pipe* OPEN_READ
                           (string-append working-directory "/bin/govuk")
                           "cuirass-jobs"))
         (jobs (match (read port)
                 ;; If an error occurred during evaluation report it,
                 ;; otherwise, suppose that data read from port are
                 ;; correct and keep things going.
                 ((? eof-object?)
                  (raise "error: govuk-cuirass-jobs: eof"))
                 (data data))))
    (close-pipe port)
    (map (lambda (job)
           (lambda () job))
         jobs)))
