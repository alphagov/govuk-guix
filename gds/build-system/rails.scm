(define-module (gds build-system rails)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages node)
  #:export (%rails-build-system-modules
            rails-build
            rails-build-system))

(define %rails-build-system-modules
  ;; Build side modules imported by default
  `((guix build syscalls)
    ,@%gnu-build-system-modules
    (gds build rails-build-system)))

(define (default-ruby)
  (let ((ruby-mod (resolve-interface '(gnu packages ruby))))
    (module-ref ruby-mod 'ruby)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (ruby (default-ruby))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("ruby" ,ruby)
                         ("node" ,node) ;; Rails seems to have a
                                        ;; transtive dependency on
                                        ;; node, or some Javascript
                                        ;; interpreter
                         ,@native-inputs))
         (outputs outputs)
         (build rails-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (rails-build store name inputs
                      #:key
                      (phases '(@ (gds build rails-build-system)
                                  %standard-phases))
                      (imported-modules %rails-build-system-modules)
                      (system (%current-system))
                      (modules '((gds build rails-build-system)
                                 (guix build utils)))
                      (search-paths '())
                      (outputs '("out"))
                      (precompile-rails-assets? #t)
                      (guile #f))
  "Build SOURCE with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (rails-build #:name ,name
                    #:source ,(match (assoc-ref inputs "source")
                                (((? derivation? source))
                                 (derivation->output-path source))
                                ((source)
                                 source)
                                (source
                                 source))
                    #:system ,system
                    #:search-paths ',(map search-path-specification->sexp
                                          search-paths)
                    #:phases ,phases
                    #:system ,system
                    #:outputs %outputs
                    #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                               ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define rails-build-system
  (build-system
    (name 'rails)
    (description "Build system for Rails applications")
    (lower lower)))
