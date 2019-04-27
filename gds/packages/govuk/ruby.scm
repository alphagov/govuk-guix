(define-module (gds packages govuk ruby)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rails)
  #:use-module (gds packages third-party ruby))

(define-public ruby-gds-sso
  (package
    (name "ruby-gds-sso")
    (version "14.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "gds-sso" version))
       (sha256
        (base32
         "1vyq7s4x110zr5dvw3xlh5h2m03823h9mmg3l05qjjiy98i93wa2"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f))
    (propagated-inputs
     `(("ruby-multi-json" ,ruby-multi-json)
       ("ruby-oauth2" ,ruby-oauth2)
       ("ruby-omniauth" ,ruby-omniauth)
       ("ruby-omniauth-gds" ,ruby-omniauth-gds)
       ("ruby-rails" ,ruby-rails)
       ("ruby-warden" ,ruby-warden)
       ("ruby-warden-oauth2" ,ruby-warden-oauth2)))
    (synopsis "Client for GDS' OAuth 2-based SSO")
    (description "Client for GDS' OAuth 2-based SSO")
    (home-page "https://github.com/alphagov/gds-sso")
    (license license:expat)))

(define-public ruby-govuk-admin-template
  (package
    (name "ruby-govuk-admin-template")
    (version "6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "govuk_admin_template" version))
       (sha256
        (base32
         "0z14lf1xf7ynarn0kf72rgdjfnhwd56yqzbwzwvp9xgk8mwcmlsf"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f))
    (propagated-inputs
     `(("ruby-bootstrap-sass" ,ruby-bootstrap-sass)
       ("ruby-jquery-rails" ,ruby-jquery-rails)
       ("ruby-rails" ,ruby-rails)))
    (synopsis
     "Styles, scripts and templates for GOV.UK admin applications")
    (description
     "Styles, scripts and templates for GOV.UK admin applications")
    (home-page
     "https://github.com/alphagov/govuk_admin_template")
    (license #f)))

(define-public ruby-govuk-document-types
  (package
    (name "ruby-govuk-document-types")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "govuk_document_types" version))
       (sha256
        (base32
         "05hwrv3n84gy2726dg9cyzqk1m6ib8xhphsdcppjw2f6jklda7ss"))))
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
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "govuk_taxonomy_helpers" version))
       (sha256
        (base32
         "0sshqz1viwm6drql9rcfk7056ajxw5vnfpv5m3zs90ab0miphin7"))))
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

(define-public ruby-omniauth-gds
  (package
    (name "ruby-omniauth-gds")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "omniauth-gds" version))
       (sha256
        (base32
         "1nkqxkn0fqnllgn34v54y33vy812pxwkzavgzi2a4a6rnpfbj4pv"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'loosen-gemspec-dependencies
           (lambda _
             (substitute* "omniauth-gds.gemspec"
               (("%q<omniauth-oauth2>.freeze, \\[.*")
                "%q<omniauth-oauth2>.freeze, ['~> 1'])\n"))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)))
    (propagated-inputs
     `(("ruby-multi-json" ,ruby-multi-json)
       ("ruby-omniauth-oauth2" ,ruby-omniauth-oauth2)))
    (synopsis "Omniauth strategy for GDS oauth2 provider")
    (description
     "Omniauth strategy for GDS oauth2 provider.")
    (home-page "")
    (license #f)))

(define-public ruby-plek
  (package
    (name "ruby-plek")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "plek" version))
       (sha256
        (base32
         "1l34xdi7n43wpbfqai656zsiwlilrnfb4kwayyscnf7gi7mfydj5"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ;; No tests included
    (synopsis
     "Find the right hostname for each service in an environment-dependent manner")
    (description
     "Find the right hostname for each service in an environment-dependent manner")
    (home-page "https://github.com/alphagov/plek")
    (license license:expat)))
