(define-module (gds services utils databases)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin)
                #:select (shadow sudo))
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (guix gcrypt)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gds packages govuk)
  #:use-module (gds services)
  #:use-module (gds services utils databases postgresql)
  #:use-module (gds services utils databases mysql)
  #:use-module (gds services utils databases mongodb)
  #:use-module (gds services utils databases elasticsearch)
  #:export (<redis-connection-config>
            redis-connection-config
            redis-connection-config?
            redis-connection-config-port
            redis-connection-config-namespace

            database-connection-config?
            database-connection-config->environment-variables
            update-database-connection-config-port
            setup-blank-databases-on-service-startup))

(define-record-type* <redis-connection-config>
  redis-connection-config make-redis-connection-config
  redis-connection-config?
  (host redis-connection-config-host
        (default "localhost"))
  (port redis-connection-config-port
        (default 6379))
  (db-number redis-connection-config-db-number
             (default 0))
  (namespace redis-connection-config-namespace
             (default #f)))

(define (database-connection-config? config)
  (or (postgresql-connection-config? config)
      (mysql-connection-config? config)
      (mongodb-connection-config? config)
      (redis-connection-config? config)))

(define database-connection-config->environment-variables
  (match-lambda
    (($ <postgresql-connection-config> host user port database)
     `(("DATABASE_URL" .
        ,(simple-format
          #f
          "postgres://~A@~A:~A/~A"
          user
          host
          port
          database))))
    (($ <mysql-connection-config> host user port database)
     `(("DATABASE_URL" .
        ,(simple-format
          #f
          "mysql2://~A:~A/~A"
          host
          port
          database))))
    (($ <mongodb-connection-config> user password host port database)
     `(("MONGO_URL" . ,host)
       ("MONGO_DB" . ,database)
       ("MONGODB_URI" .
        ,(simple-format
          #f
          "mongodb://~A:~A/~A"
          host
          port
          database))))
    (($ <redis-connection-config> host port db-number namespace)
     `(("REDIS_URL" .
        ,(simple-format
          #f
          "redis://~A:~A/~A"
          host
          port
          db-number))
       ("REDIS_HOST" . ,host)
       ("REDIS_PORT" . ,(number->string port))
       ,@(if namespace
             `("REDIS_NAMESPACE" . ,namespace)
             '())))
    (unmatched
     (error "get-database-environment-variables no match for ~A"
            unmatched))))

(define (update-database-connection-config-port port-for config)
  (cond
   ((postgresql-connection-config? config)
    (postgresql-connection-config
     (inherit config)
     (port (port-for 'postgresql))))
   ((mysql-connection-config? config)
    (mysql-connection-config
     (inherit config)
     (port (port-for 'mysql))))
   ((mongodb-connection-config? config)
    (mongodb-connection-config
     (inherit config)
     (port (port-for 'mongodb))))
   ((elasticsearch-connection-config? config)
    (elasticsearch-connection-config
     (inherit config)
     (port (port-for 'elasticsearch))))
   ((redis-connection-config? config)
    (redis-connection-config
     (inherit config)
     (port (port-for 'redis))))
   (else (error "unknown database connection config " config))))

(define (setup-blank-databases-on-service-startup s)
  (let
      ((parameters (service-parameters s)))
    (if (not (list? parameters))
        s
        (let* ((ssc (find service-startup-config? parameters))
               (database-connection-configs
                (filter database-connection-config? parameters)))
          (service
           (service-kind s)
           (map
            (lambda (parameter)
              (if (service-startup-config? parameter)
                  (service-startup-config-add-pre-startup-scripts
                   parameter
                   (concatenate
                    (map
                     (lambda (config)
                       (cond
                        ((postgresql-connection-config? config)
                         `((postgresql
                            .
                            ,(postgresql-create-user-and-database-for-database-connection
                              config))))
                        ((mysql-connection-config? config)
                         `((mysql
                            .
                            ,(mysql-create-user-and-database-for-database-connection
                              config))))
                        ((mongodb-connection-config? config)
                         '()) ;; TODO
                        ((redis-connection-config? config)
                         '()) ;; redis does not require any setup
                        (else
                         (error "Unrecognised database config"))))
                     database-connection-configs))
                   #:run-as-root #t)
                  parameter))
            parameters))))))
