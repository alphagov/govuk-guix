(use-modules (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim))

(define working-directory
  (getcwd))

(define (govuk-packages-jobs . args)
  (let* ((port (open-pipe* OPEN_READ
                           (string-append working-directory "/bin/govuk")
                           "cuirass-jobs"))
         (jobs (match (read port)
                 ;; If an error occured during evaluation report it,
                 ;; otherwise, suppose that data read from port are
                 ;; correct and keep things going.
                 ((? eof-object?)
                  (raise "error: govuk-cuirass-jobs: eof"))
                 (data data))))
    (close-pipe port)
    (map (lambda (job)
           (lambda () job))
         jobs)))
