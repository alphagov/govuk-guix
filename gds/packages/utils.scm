(define-module (gds packages utils)
  #:use-module (ice-9 match)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:export (github-archive))

(define* (github-archive
          #:optional #:key
          repository
          commit-ish
          (user-or-org "alphagov")
          (url (if repository
                (string-append
                 "https://github.com/"
                 user-or-org "/"
                 repository "/archive/"
                 commit-ish ".tar.gz")
                #f))
          (hash-algo 'sha256)
          (hash #f))
  (if (not url)
      (error "Either repository, or the full url must be specified"))
  (origin
    (method url-fetch)
    (uri url)
    (sha256 hash)))

(define-public create-bin-bundle
  (lambda ()
    `(lambda* (#:key inputs outputs #:allow-other-keys)
       (let*
           ((out (assoc-ref outputs "out"))
            (gemfile (string-append out "/Gemfile"))
            (ruby
             (string-append (assoc-ref inputs "ruby")
                            "/bin/ruby")))
         (define* (bundle ruby-path #:optional (port #f))
           (format port "#!~A
ENV[\"BUNDLE_GEMFILE\"] ||= \"~A\"

load Gem.bin_path(\"bundler\", \"bundler\")" ruby-path gemfile))

         (mkdir-p (string-append out "/bin"))
         (call-with-output-file (string-append out "/bin/bundle")
           (lambda (port)
             (bundle ruby port)))
         (chmod (string-append out "/bin/bundle") #o544)
         #t))))

(define-public replace-mongoid.yml
  (lambda* (#:key (mongoid-version "4")
                  (path "/config/mongoid.yml"))
    `(lambda* (#:key outputs #:allow-other-keys)
       (let ((location
              (string-append
               (assoc-ref outputs "out") ,path))
             (clients-or-sessions
              (if (equal? ,mongoid-version "3")
                  "sessions"
                  "clients")))
         (delete-file location)
         (call-with-output-file location
           (lambda (port)
             (simple-format port "
development:
  ~A:
    default:
      uri: <%= ENV['MONGODB_URI'] %>
      options:
        write:
          w: majority

test:
  ~A:
    default:
      uri: <%= ENV['MONGODB_URI'] %>
      options:
        write:
          w: majority

production:
  ~A:
    default:
      uri: <%= ENV['MONGODB_URI'] %>
      options:
        write:
          w: majority
" clients-or-sessions clients-or-sessions clients-or-sessions)))
         #t))))

(define-public replace-redis.yml
  (lambda ()
    `(lambda* (#:key outputs #:allow-other-keys)
       (let ((location
              (string-append
               (assoc-ref outputs "out")
               "/config/redis.yml")))
         (delete-file location)
         (call-with-output-file location
           (lambda (port)
             (simple-format port "
host: <%= ENV['REDIS_HOST'] %>
port: <%= ENV['REDIS_PORT'] %>
namespace: <%= ENV['REDIS_NAMESPACE'] %>
")))
         #t))))

(define-public replace-database.yml
  (lambda ()
    `(lambda* (#:key outputs #:allow-other-keys)
       (let ((location
              (string-append
               (assoc-ref outputs "out")
               "/config/database.yml")))
         (delete-file location)
         (call-with-output-file location
           (lambda (port)
             (simple-format port "
development:
  url: <%= ENV['DATABASE_URL'] %>

test:
  url: <%= ENV['DATABASE_URL'] %>

production:
  url: <%= ENV['DATABASE_URL'] %>
")))
         #t))))

(define-public (replace-ruby-version version)
  `(lambda* (#:key outputs #:allow-other-keys)
     (let ((location
            (string-append
             (getcwd)
             "/.ruby-version")))
       (if (file-exists? location)
           (delete-file location))
       (call-with-output-file location
         (lambda (port)
           (simple-format port "~A" ,version)))
       #t)))

(define-public replace-gds-sso-initializer
  (lambda ()
    `(lambda* (#:key outputs #:allow-other-keys)
       (let ((location
              (string-append
               (assoc-ref outputs "out")
               "/config/initializers/gds-sso.rb")))
         (delete-file location)
         (call-with-output-file location
           (lambda (port)
             (simple-format port "
GDS::SSO.config do |config|
  config.user_model   = 'User'

  config.oauth_id     = ENV['OAUTH_ID']
  config.oauth_secret = ENV['OAUTH_SECRET']

  config.oauth_root_url = Plek.find('signon')
end
")))
         #t))))

(define-public use-blank-database.yml
  (lambda ()
    `(lambda* (#:key outputs #:allow-other-keys)
       (let ((location
              (string-append
               (assoc-ref outputs "out")
               "/config/database.yml")))
         (delete-file location)
         (call-with-output-file location
           (lambda (port)
             (simple-format port "")))
         #t))))

(define-public (package-with-ruby ruby pkg)
  (package
   (inherit pkg)
   (inputs
    (map
     (match-lambda
       ((name pkg rest ...)
        (if (equal? name "ruby")
            `("ruby" ,ruby)
            (cons* name pkg rest))))
     (package-inputs pkg)))))
