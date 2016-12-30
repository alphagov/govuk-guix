(define-module (gds packages utils)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix build utils))

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
  (lambda* (#:key (mongoid-version "4"))
    `(lambda* (#:key outputs #:allow-other-keys)
       (let ((location
              (string-append
               (assoc-ref outputs "out")
               "/config/mongoid.yml"))
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
