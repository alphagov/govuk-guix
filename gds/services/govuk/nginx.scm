(define-module (gds services govuk nginx)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (gnu services web)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gds services govuk router)
  #:use-module (gds services govuk tls)
  #:export (<govuk-nginx-configuration>
            govuk-nginx-configuration
            govuk-nginx-configuration?
            make-govuk-nginx-configuration
            govuk-nginx-configuration-http-port
            govuk-nginx-configuration-https-port
            govuk-nginx-configuration-service-and-ports
            govuk-nginx-configuration-origin-service
            govuk-nginx-configuration-draft-origin-service
            govuk-nginx-configuration-server-aliases
            govuk-nginx-configuration-web-domain
            govuk-nginx-configuration-app-domain
            govuk-nginx-configuration-tls
            govuk-nginx-configuration-additional-server-blocks
            govuk-nginx-configuration-origin-password-file
            govuk-nginx-configuration-intercept-errors

            <password-file>
            password-file
            password-file-name
            password-file-usernames-and-passwords
            password-file-htpasswd

            govuk-nginx-service-type))

(define %services-to-proxy-assets-for
  '(asset-manager
    calculators
    calendars
    collections
    email-alert-frontend
    feedback
    finder-frontend
    frontend
    government-frontend
    info-frontend
    manuals-frontend
    licencefinder
    service-manual-frontend
    smartanswers))

(define (nginx-upstream-configurations service-and-ports)
  (map
   (match-lambda
     ((service . port)
      (nginx-upstream-configuration
       (name (string-append (symbol->string service) "-proxy"))
       (servers (list
                 (string-append "localhost:" (number->string port)))))))
   service-and-ports))

(define (proxy-set-header-host include-port-in-host-header?)
  (if include-port-in-host-header?
      "proxy_set_header Host $host:$server_port;"
      "proxy_set_header Host $host;"))

(define (error-page-locations
         app-domain
         https?
         nginx-port)
  (define (proxy-pass-to-static code)
    (simple-format #f "proxy_pass ~A://static.~A:~A/templates/~A.html.erb;"
                   (if https? "https" "http")
                   app-domain
                   nginx-port
                   code))

  (map (lambda (code)
         (nginx-location-configuration
          (uri (string-append "/" code ".html"))
          (body (list (proxy-pass-to-static code)))))
       '("404" "406" "410" "500" "503" "504")))

(define error_pages
  '("error_page 400 /400.html;"
    "error_page 404 401 /404.html;"
    "error_page 406 /406.html;"
    "error_page 410 /410.html;"
    "error_page 500 502 /500.html;"
    "error_page 503 /503.html;"
    "error_page 504 /504.html;"
    "error_page 429 /503.html;"))

(define (web-domain-server-configuration
         base-nginx-server-configuration
         origin-service
         web-domain
         app-domain
         https?
         nginx-port
         intercept-errors?)
  (nginx-server-configuration
   (inherit base-nginx-server-configuration)
   (locations
    `(,(nginx-location-configuration
        (uri "/")
        (body (list (simple-format
                     #f "proxy_set_header GOVUK-Request-Id $pid-$msec-$remote_addr-$request_length;
proxy_pass http://~A-proxy;" origin-service))))
      ,(nginx-location-configuration
        (uri "/api/content")
        (body '("proxy_pass http://content-store-proxy;")))
      ,@(error-page-locations
         app-domain
         https?
         nginx-port)))
   (raw-content (append
                 (if intercept-errors?
                     '("proxy_intercept_errors on;")
                     '())
                 error_pages
                 (nginx-server-configuration-raw-content
                  base-nginx-server-configuration)))
   (server-name (list (string-append web-domain)))))

(define (draft-origin-server-configuration
         base-nginx-server-configuration
         draft-origin-service
         app-domain
         https?
         include-port-in-host-header?
         nginx-port
         intercept-errors?)
  (nginx-server-configuration
   (inherit base-nginx-server-configuration)
   (locations
    `(,(nginx-location-configuration
        (uri "/")
        (body `(,(simple-format
                  #f "proxy_set_header GOVUK-Request-Id $pid-$msec-$remote_addr-$request_length;
proxy_pass http://~A-proxy;\n" draft-origin-service)
                ,(proxy-set-header-host include-port-in-host-header?)
                ,@(if https?
                      '("# Set X-Forwarded-SSL for OmniAuth"
                        "proxy_set_header X-Forwarded-SSL 'on';")
                      '()))))
      ;; TODO: This should be reworked somehow, to add
      ;; authentication. Maybe a special route could route
      ;; /api/content directly through to the Content Store?
      ,(nginx-location-configuration
        (uri "/api/content")
        (body '("proxy_pass http://draft-content-store-proxy;")))
      ,@(error-page-locations
         app-domain
         https?
         nginx-port)))
   (raw-content (append
                 (if intercept-errors?
                     '("proxy_intercept_errors on;")
                     '())
                 error_pages))
   (server-name (list (string-append "draft-origin." app-domain)))))

(define (assets-server-configuration
         services
         base-nginx-server-configuration
         app-domain
         nginx-port
         https?
         include-port-in-host-header?
         asset-manager-service-available?)

  (define special-cases
    '((whitehall-frontend
       . ("/government/assets/"
          "/government/placeholder"
          "/government/uploads/"))
      (static
       . ("/government/uploads/system/uploads/organisation/logo/"
          "/government/uploads/system/uploads/classification_featuring_image_data/file/"
          "/government/uploads/system/uploads/consultation_response_form_data/file/"
          "/government/uploads/system/uploads/default_news_organisation_image_data/file/"
          "/government/uploads/system/uploads/person/image/"
          "/government/uploads/system/uploads/promotional_feature_item/image/"
          "/government/uploads/system/uploads/take_part_page/image/"
          "/government/uploads/system/uploads/image_data/file/"))))

  (define (proxy-pass-to-service service)
    (simple-format #f "proxy_pass ~A://~A.~A:~A;"
                   (if https? "https" "http")
                   service
                   app-domain
                   nginx-port))

  (define access-control-headers
    '("add_header \"Access-Control-Allow-Origin\" \"*\";"
      "add_header \"Access-Control-Allow-Methods\" \"GET, OPTIONS\";"
      "add_header \"Access-Control-Allow-Headers\" \"origin, authorization\";"))

  (nginx-server-configuration
   (inherit base-nginx-server-configuration)
   (locations
    (append
     (if asset-manager-service-available?
         (list
          (nginx-location-configuration
           (uri "/media")
           (body `(,@access-control-headers
                   "proxy_pass http://asset-manager-proxy;")))
          (nginx-location-configuration
           (uri "~ /fake-s3/(.*)")
           (body `(,(proxy-set-header-host include-port-in-host-header?)
                   "proxy_pass http://asset-manager-proxy;"))))
         '())
     (list
      (nginx-location-configuration
       (uri "~ /cloud-storage-proxy/(.*)")
       (body '("internal;"
               "set $download_url $1$is_args$args;"
               "proxy_pass $download_url;"))))
     (append-map
      (match-lambda
        ((service . locations)
         (map (lambda (location)
                (nginx-location-configuration
                 (uri location)
                 (body
                  `(,@access-control-headers
                    ,(proxy-pass-to-service service)))))
              locations)))
      special-cases)
     (map
      (lambda (service)
        (nginx-location-configuration
         (uri (simple-format #f "/~A" service))
         (body `(,@access-control-headers
                 ,(proxy-pass-to-service service)))))
      services)
     (list
      (nginx-location-configuration
       (uri "/")
       (body `(,@access-control-headers
               ,(proxy-pass-to-service "static")))))))
    (server-name (list (string-append "assets." app-domain)))))

(define (service-nginx-server-configuration
         base-nginx-server-configuration
         app-domain
         server-aliases
         service
         port
         https?
         include-port-in-host-header?)
  (nginx-server-configuration
   (inherit base-nginx-server-configuration)
   (locations
    `(,(nginx-location-configuration
        (uri "/")
        (body (list "try_files $uri/index.html $uri.html $uri @app;")))
      ,(nginx-named-location-configuration
        (name "app")
        (body
         `(,(simple-format #f "access_log /var/log/nginx/~A.access.log;" service)
           "proxy_set_header GOVUK-Request-Id $pid-$msec-$remote_addr-$request_length;"
           ,(simple-format #f "proxy_pass http://~A-proxy;" service)
           ,@(if https?
                 '("# Set X-Forwarded-SSL for OmniAuth"
                   "proxy_set_header X-Forwarded-SSL 'on';")
                 '())
           ,(proxy-set-header-host include-port-in-host-header?))))
      ,@(if (eq? service 'static)
            (list
             ;; This is a special location which is used for analytics
             (nginx-location-configuration
              (uri "/static/a")
              (body (list "expires -1;
add_header Last-Modified \"\";

default_type text/plain;
return 200 '';"))))
            '())
      ,@(if (eq? service 'whitehall)
            (list
             (nginx-location-configuration
              (uri "/government/uploads")
              (body (list (simple-format
                           #f
                           "proxy_pass http://whitehall-proxy;
proxy_set_header Host whitehall-admin.~A~A;"
                           app-domain
                           (if include-port-in-host-header?
                               ":$server_port"
                               ""))))))
            '())))
   (server-name (map
                 (lambda (name)
                   (simple-format #f "~A.~A" name app-domain))
                 (cons service
                       (or (assq-ref server-aliases service)
                           '()))))
   (root (string-append "/var/apps/" (symbol->string service) "/public"))))

(define* (nginx-server-configurations base-nginx-server-configuration
                                      service-and-ports
                                      server-aliases
                                      origin-service
                                      draft-origin-service
                                      web-domain
                                      app-domain
                                      nginx-port
                                      origin-password-file
                                      #:key https?
                                      include-port-in-host-header?
                                      intercept-errors?)
  (define base-nginx-server-configuration-for-origin
    (if origin-password-file
        (nginx-server-configuration
         (inherit base-nginx-server-configuration)
         (raw-content
          `("auth_basic \"Access Restricted\";"
            ("auth_basic_user_file " ,origin-password-file ";"))))
        base-nginx-server-configuration))

  (filter
   nginx-server-configuration?
   (cons*
    (if origin-service
        (web-domain-server-configuration base-nginx-server-configuration-for-origin
                                         origin-service
                                         web-domain
                                         app-domain
                                         https?
                                         nginx-port
                                         intercept-errors?))
    (if draft-origin-service
        (draft-origin-server-configuration base-nginx-server-configuration
                                           draft-origin-service
                                           app-domain
                                           https?
                                           include-port-in-host-header?
                                           nginx-port
                                           intercept-errors?))
    (assets-server-configuration (let ((services (map car service-and-ports)))
                                   (filter (lambda (service)
                                             (memq service services))
                                           %services-to-proxy-assets-for))
                                 base-nginx-server-configuration
                                 app-domain
                                 nginx-port
                                 https?
                                 include-port-in-host-header?
                                 (memq 'asset-manager
                                       (map car
                                            service-and-ports)))
    (map
     (match-lambda
       ((service . port)
        (service-nginx-server-configuration
         base-nginx-server-configuration
         app-domain
         server-aliases
         service
         port
         https?
         include-port-in-host-header?)))
     service-and-ports))))

(define-record-type* <govuk-nginx-configuration>
  govuk-nginx-configuration make-govuk-nginx-configuration
  govuk-nginx-configuration?
  (http-port                      govuk-nginx-configuration-http-port
                                  (default 80))
  (https-port                     govuk-nginx-configuration-https-port
                                  (default 443))
  ;; This is useful when the intended port to connect to nginx on,
  ;; isn't the port it's listening on, e.g. if it's running on 8080,
  ;; but traffic comes in on port 80, and is redirected by iptables.
  ;;
  ;; This is a substitute for more complex network handling.
  (include-port-in-host-header?   govuk-nginx-configuration-include-port-in-host-header?
                                  (default #f))
  (service-and-ports              govuk-nginx-configuration-service-and-ports
                                  (default '()))
  (origin-service                 govuk-nginx-configuration-origin-service
                                  (default 'router))
  (draft-origin-service           govuk-nginx-configuration-draft-origin-service
                                  (default 'authenticating-proxy))
  (server-aliases                 govuk-nginx-configuration-server-aliases
                                  (default '()))
  (web-domain                     govuk-nginx-configuration-web-domain
                                  (default "www.gov.uk"))
  (app-domain                     govuk-nginx-configuration-app-domain
                                  (default "publishing.service.gov.uk"))
  (tls                            govuk-nginx-configuration-tls
                                  (default #f))
  (additional-nginx-server-blocks govuk-nginx-configuration-additional-server-blocks
                                  (default '()))
  (origin-password-file           govuk-nginx-configuration-origin-password-file
                                  (default #f))
  (intercept-errors               govuk-nginx-configuration-intercept-errors
                                  (default #t)))

(define-record-type* <password-file>
  password-file make-password-file
  password-file?
  (name password-file-name
        (default "password-file"))
  (usernames-and-passwords password-file-usernames-and-passwords)
  (htpasswd password-file-htpasswd
            (default (file-append httpd "/bin/htpasswd"))))

(define-gexp-compiler (password-file-compiler
                       (record <password-file>) system target)
  (match record
    (($ <password-file> name usernames-and-passwords htpasswd)
     (gexp->derivation
      (string-append "make-" name)
      #~(let* ((output (ungexp output "out"))
               (usernames-and-passwords '#$usernames-and-passwords)
               (first-user (car usernames-and-passwords))
               (rest (cdr usernames-and-passwords)))
          (define* (store-password username password #:key (create? #f))
            (apply system*
                   `(#$htpasswd
                     ,@(if create?
                           '("-c")
                           '())
                     "-b"
                     ,output
                     ,username
                     ,password)))

          (store-password (car first-user) (cdr first-user)
                          #:create? #t)
          (for-each (lambda (username-and-password)
                      (store-password (car username-and-password)
                                      (cdr username-and-password)))
                    rest))
      #:local-build? #t))))

(define (apply-base-nginx-server-configuration
         govuk-nginx-config nginx-server-config)

  (define tls-config
    (govuk-nginx-configuration-tls govuk-nginx-config))

  (nginx-server-configuration
   (inherit nginx-server-config)
   (listen
    `(,(number->string (govuk-nginx-configuration-http-port govuk-nginx-config))
      ,@(or (and=> (govuk-nginx-configuration-https-port govuk-nginx-config)
                   (lambda (https-port)
                     (list (string-append (number->string https-port) " ssl"))))
            '())))
   (ssl-certificate (cond
                     ((eq? tls-config 'development)
                      "/etc/nginx/cert")
                     ((eq? tls-config 'certbot)
                      (string-append "/etc/letsencrypt/live/"
                                     (govuk-nginx-configuration-web-domain
                                      govuk-nginx-config)
                                     "/fullchain.pem"))
                     (else #f)))
   (ssl-certificate-key (cond
                         ((eq? tls-config 'development)
                          "/etc/nginx/key")
                         ((eq? tls-config 'certbot)
                          (string-append "/etc/letsencrypt/live/"
                                         (govuk-nginx-configuration-web-domain
                                          govuk-nginx-config)
                                         "/privkey.pem"))
                         (else #f)))))

(define (base-nginx-server-configuration govuk-nginx-config)
  (apply-base-nginx-server-configuration govuk-nginx-config
                                         (nginx-server-configuration)))

(define (govuk-nginx-configuration->nginx-configuration config)
  (match config
    (($ <govuk-nginx-configuration> http-port
                                    https-port
                                    include-port-in-host-header?
                                    service-and-ports
                                    origin-service
                                    draft-origin-service
                                    server-aliases
                                    web-domain
                                    app-domain
                                    tls
                                    additional-nginx-server-blocks
                                    origin-password-file
                                    intercept-errors)
     (nginx-configuration
      (server-blocks
       (nginx-server-configurations (base-nginx-server-configuration config)
                                    service-and-ports
                                    server-aliases
                                    origin-service
                                    draft-origin-service
                                    web-domain
                                    app-domain
                                    (or https-port http-port)
                                    origin-password-file
                                    #:https? (number? https-port)
                                    #:include-port-in-host-header?
                                    include-port-in-host-header?
                                    #:intercept-errors? intercept-errors))
      (upstream-blocks
       (nginx-upstream-configurations service-and-ports))
      (server-names-hash-bucket-size 128)))))

(define (maybe-convert-to-nginx-configuration config)
  (if (govuk-nginx-configuration? config)
      (govuk-nginx-configuration->nginx-configuration config)
      config))

(define govuk-nginx-service-type
  (service-type
   (inherit nginx-service-type)
   (extensions
    (append
     (map (lambda (se)
            (let ((target (service-extension-target se))
                  (compute (service-extension-compute se)))
              (cond ((eq? target shepherd-root-service-type)
                     (service-extension
                      target
                      (lambda config
                        (match (apply
                                (compose (@@ (gnu services web) nginx-shepherd-service)
                                         maybe-convert-to-nginx-configuration)
                                config)
                          (((? shepherd-service? service))
                           (list
                            (shepherd-service
                             (inherit service)
                             (requirement
                              (cons 'certificates
                                    (shepherd-service-requirement service))))))))))
                    ((eq? target activation-service-type)
                     (service-extension
                      target
                      (compose (@@ (gnu services web) nginx-activation)
                               maybe-convert-to-nginx-configuration)))
                    (else (service-extension target compute)))))
          (service-type-extensions nginx-service-type))
     (list
      (service-extension
       special-files-service-type
       (lambda (nginx-config)
         `(("/etc/nginx/cert"
            ,(development-os-tls-certificate
              (append-map nginx-server-configuration-server-name
                          (nginx-configuration-server-blocks nginx-config))))
           ("/etc/nginx/key" ,development-os-tls-private-key))))
      (service-extension
       profile-service-type
       (lambda (nginx-config)
         (list
          (development-os-certificates-package-for-domains
           (append-map nginx-server-configuration-server-name
                       (nginx-configuration-server-blocks nginx-config)))))))))
   (extend (lambda (config servers)
             ((service-type-extend nginx-service-type)
              (govuk-nginx-configuration->nginx-configuration config)
              (map (lambda (server)
                     (apply-base-nginx-server-configuration config server))
                   servers))))
   (default-value (govuk-nginx-configuration))))
