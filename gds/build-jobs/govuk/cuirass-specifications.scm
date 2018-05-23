(define-module (gds build-jobs govuk cuirass-specifications)
  #:export (govuk-packages))

(define govuk-packages
  `((#:name . "govuk-packages")
    (#:url . "govuk-guix")
    (#:load-path . ".")
    (#:file . "gds/build-jobs/cuirass-entry-point.scm")
    (#:proc . govuk-packages-jobs)
    (#:arguments . ())
    (#:branch . "master")
    (#:no-compile? . 1)))

(list govuk-packages)
