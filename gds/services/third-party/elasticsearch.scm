(define-module (gds services third-party elasticsearch)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (gds packages third-party elasticsearch)
  #:export (<elasticsearch-configuration>
            elasticsearch-configuration
            elasticsearch-configuration?
            elasticsearch-configuration-elasticsearch
            elasticsearch-configuration-data-path
            elasticsearch-configuration-logs-path
            elasticsearch-configuration-port
            elasticsearch-configuration-transport-port

            elasticsearch-service
            elasticsearch-service-type))

(define-record-type* <elasticsearch-configuration>
  elasticsearch-configuration make-elasticsearch-configuration
  elasticsearch-configuration?
  (elasticsearch     elasticsearch-configuration-elasticsearch
                     (default elasticsearch))
  (data-path         elasticsearch-configuration-data-path
                     (default "/var/lib/"))
  (logs-path         elasticsearch-configuration-logs-path
                     (default "/var/log/elasticsearch"))
  (http-port         elasticsearch-configuration-port
                     (default 9200))
  (transport-port    elasticsearch-configuration-transport-port
                     (default 9300)))

(define (elasticsearch-configuration-directory
         data-path logs-path http-port transport-port)
  (computed-file
   "elasticsearch-config"
   #~(begin
       (mkdir #$output)
       (mkdir (string-append #$output "/scripts"))
       (call-with-output-file (string-append #$output "/elasticsearch.yml")
         (lambda (port)
           (display
            (string-append
             "path.data: " #$data-path "\n"
             "path.logs: " #$logs-path "\n"
             "http.port: " #$(number->string http-port) "\n"
             "transport.tcp.port: " #$(number->string transport-port) "\n")
            port))))))

(define %elasticsearch-accounts
  (list (user-group (name "elasticsearch") (system? #t))
        (user-account
         (name "elasticsearch")
         (group "elasticsearch")
         (system? #t)
         (comment "Elasticsearch server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define elasticsearch-activation
  (match-lambda
    (($ <elasticsearch-configuration> elasticsearch data-path logs-path
                                      http-port transport-port)
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match))

         (let ((user (getpwnam "elasticsearch")))
           ;; Create db state directory.
           (for-each
            (lambda (path)
              (mkdir-p path)
              (chown path (passwd:uid user) (passwd:gid user)))
            '(#$data-path #$logs-path "/var/run/elasticsearch")))))))

(define elasticsearch-shepherd-service
  (match-lambda
    (($ <elasticsearch-configuration> elasticsearch data-path logs-path
                                      http-port transport-port)
     (list (shepherd-service
            (provision '(elasticsearch))
            (documentation "Run the Elasticsearch daemon.")
            (requirement '(user-processes syslogd))
            (start #~(make-forkexec-constructor
                      (list
                       (string-append #$elasticsearch "/bin/elasticsearch")
                       "-d"
                       "-p" "/var/run/elasticsearch/pid"
                       (string-append
                        "-Dpath.conf="
                        #$(elasticsearch-configuration-directory
                           data-path logs-path http-port transport-port)))
                      #:user "elasticsearch"
                      #:pid-file "/var/run/elasticsearch/pid"
                      #:log-file "/var/log/elasticsearch.log"))
            (stop #~(make-kill-destructor)))))))

(define elasticsearch-service-type
  (service-type (name 'elasticsearch)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          elasticsearch-shepherd-service)
                       (service-extension activation-service-type
                                          elasticsearch-activation)
                       (service-extension account-service-type
                                          (const %elasticsearch-accounts))))
                (default-value (elasticsearch-configuration))))
