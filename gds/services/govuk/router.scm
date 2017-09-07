(define-module (gds services govuk router)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gds services utils databases)
  #:export (<router-config>
            router-config
            router-config?
            router-config-public-port
            router-config-api-port

            router-config->environment-variables
            make-router-shepherd-service
            make-router-service-type))

(define-record-type* <router-config>
  router-config make-router-config
  router-config?
  (public-port router-config-public-port
               (default 8080))
  (api-port router-config-api-port
            (default 8081))
  (debug? router-config-debug
          (default #f))
  (backend-connect-timeout router-config-backend-connect-timeout
                           (default "1s"))
  (backend-header-timeout router-config-backend-header-timeout
                          (default "15s")))

(define router-config->environment-variables
  (match-lambda
    (($ <router-config> public-port api-port debug?)
     (append
      (list
       (cons "ROUTER_PUBADDR" (simple-format #f ":~A" public-port))
       (cons "ROUTER_APIADDR" (simple-format #f ":~A" api-port)))
      (if debug?
          (list (cons "DEBUG" "true"))
          '())))))

(define (make-router-shepherd-service name)
  (match-lambda
    ((router-config package rest ...)
     (let ((environment-variables
            (map
             (match-lambda
              ((key . value)
               (string-append key "=" value)))
             (map
              (match-lambda
               ((name . value)
                (cond
                 ((string=? name "MONGO_DB")
                  (cons "ROUTER_MONGO_DB" value))
                 ((string=? name "MONGODB_URI")
                  (cons "ROUTER_MONGO_URL" value))
                 (else
                  (cons name value)))))
              (append
               (router-config->environment-variables router-config)
               (concatenate
                (map database-connection-config->environment-variables
                     (filter database-connection-config? rest)))))))
           (user (getpwnam "nobody"))
           (string-service-name
            (symbol->string name)))
       (list
        (shepherd-service
         (provision (list name))
         (documentation string-service-name)
         (requirement '(mongodb))
         (respawn? #f)
         (start #~(make-forkexec-constructor
                   (string-append #$package "/bin/router")
                   #:user (passwd:uid #$user)
                   #:environment-variables '#$environment-variables
                   #:log-file (string-append "/var/log/" #$string-service-name)))
         (stop #~(make-kill-destructor))))))))

(define (make-router-service-type name)
  (service-type
   (name name)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (make-router-shepherd-service name))))))
