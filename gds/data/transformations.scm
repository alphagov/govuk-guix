(define-module (gds data transformations)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:export (<data-transformation>
            data-transformation
            data-transformation?
            data-transformation-output-name
            data-transformation-output-description
            data-transformation-operation

            <multi-output-data-transformation>
            multi-output-data-transformation
            multi-output-data-transformation?
            multi-output-data-transformation-name
            multi-output-data-transformation-outputs
            multi-output-data-transformation-operation

            <gexp-output-alias>
            gexp-output-alias
            gexp-output-alias
            gexp-output-alias-gexp
            gexp-output-alias-output))

(define-record-type* <data-transformation>
  data-transformation make-data-transformation
  data-transformation?
  (output-name data-transformation-output-name)
  (output-description data-transformation-output-description
                      (default #f))
  (operation data-transformation-operation))

(define-gexp-compiler (data-transformation-compiler
                       (data-transformation <data-transformation>)
                       system target)
  (match data-transformation
    (($ <data-transformation> output-name output-description
                              operation)

     (gexp->derivation output-name operation
                       #:system system))))

(define-record-type* <multi-output-data-transformation>
  multi-output-data-transformation make-multi-output-data-transformation
  multi-output-data-transformation?
  (name      multi-output-data-transformation-name)
  (outputs   multi-output-data-transformation-outputs)
  (operation multi-output-data-transformation-operation))

(define-gexp-compiler (multi-output-data-transformation-compiler
                       (multi-output-data-transformation
                        <multi-output-data-transformation>)
                       system target)
  (match multi-output-data-transformation
    (($ <multi-output-data-transformation> name outputs operation)

     (gexp->derivation name
                       operation
                       #:system system))))

(define-record-type* <gexp-output-alias>
  gexp-output-alias make-gexp-output-alias
  gexp-output-alias?
  (gexp   gexp-output-alias-gexp)
  (output gexp-output-alias-output))

(define-gexp-compiler (gexp-output-alias-compiler
                       (gexp-output-alias <gexp-output-alias>)
                       system target)
  (match gexp-output-alias
    (($ <gexp-output-alias> aliased-gexp output-name)

     (gexp->derivation (string-append "output-alias-" output-name)
                       #~(symlink (ungexp aliased-gexp output-name)
                                  #$output)
                       #:system system))))
