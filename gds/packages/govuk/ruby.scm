(define-module (gds packages govuk ruby)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages ruby))

(define-public ruby-govuk-document-types
  (package
    (name "ruby-govuk-document-types")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "govuk_document_types" version))
       (sha256
        (base32
         "1pngahhmqmqwqb4n98zf9v0y03xdzl14rnpllmsy816v0f4c1mml"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Gem to share document type groupings")
    (description
     "Gem to share document type groupings")
    (home-page
     "https://github.com/alphagov/govuk_document_types")
    (license license:expat)))

(define-public ruby-govuk-taxonomy-helpers
  (package
    (name "ruby-govuk-taxonomy-helpers")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "govuk_taxonomy_helpers" version))
       (sha256
        (base32
         "0bgydkscj03121pg47msyhwyim67lhzxlj7kkjjr6m263j8r03gp"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis
     "Parses the taxonomy of GOV.UK into a browseable tree structure.")
    (description
     "Parses the taxonomy of GOV.UK into a browseable tree structure.")
    (home-page
     "https://github.com/alphagov/govuk_taxonomy_helpers")
    (license license:expat)))

(define-public ruby-plek
  (package
    (name "ruby-plek")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "plek" version))
       (sha256
        (base32
         "189aryw1rcvnfxpdnvpz9qn8c0vf658mlhgyvyi1yrxrdy2z14i7"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ;; No tests included
    (synopsis
     "Find the right hostname for each service in an environment-dependent manner")
    (description
     "Find the right hostname for each service in an environment-dependent manner")
    (home-page "https://github.com/alphagov/plek")
    (license license:expat)))
