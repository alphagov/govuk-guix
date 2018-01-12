(define-module (gds systems govuk utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (gds services utils databases)
  #:use-module (gds services utils databases elasticsearch)
  #:use-module (gds services utils databases mongodb)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds services utils databases postgresql)
  #:export (govuk-skeletons-service-type))

(define (govuk-skeletons redis-connection-config
                         memcached-connection-config
                         postgresql-connection-config)
  (define pair->alias
    (match-lambda
      ((name . value)
       (simple-format #f "alias ~A=\"~A\"\n" name value))))

  `((".psqlrc" ,(local-file "skeletons/psqlrc"))
    (".bashrc" ,(local-file "skeletons/bashrc"))
    (".bash_aliases"
     ,(plain-file
       "aliases"
       (string-concatenate
        (map pair->alias
             `(("redis" .
                ,(string-append
                  "redis-cli -p "
                  (number->string
                   (redis-connection-config-port redis-connection-config))))
               ("memcached-telnet" .
                ,(string-append
                  "telnet "
                  (number->string
                   (memcached-connection-config-port memcached-connection-config)))))))))
    (".environment"
     ,(plain-file
       "environment"
       (apply
        string-append
        (map (match-lambda
               ((name . value)
                (simple-format #f "export ~A=~A\n" name value)))
             `(("PGPORT" . ,(postgresql-connection-config-port
                             postgresql-connection-config))
               ("PGUSER" . "postgres"))))))))

(define govuk-skeletons-service-type
  (service-type
   (name 'govuk-skeletons)
   (extensions
    (list
     (service-extension account-service-type
                        (lambda (parameters)
                          (govuk-skeletons
                           (find redis-connection-config? parameters)
                           (find memcached-connection-config? parameters)
                           (find postgresql-connection-config? parameters))))))
   (default-value
     (list
      (redis-connection-config)
      (memcached-connection-config)
      (postgresql-connection-config
       (user #f) (database #f))))))
