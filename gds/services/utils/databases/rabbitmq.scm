(define-module (gds services utils databases rabbitmq)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages rabbitmq)
  #:export (<rabbitmq-connection-config>
            rabbitmq-connection-config
            rabbitmq-connection-config?
            rabbitmq-connection-config-hosts
            rabbitmq-connection-config-vhost
            rabbitmq-connection-config-user
            rabbitmq-connection-config-password

            rabbitmq-create-user-for-connection-config))

(define-record-type* <rabbitmq-connection-config>
  rabbitmq-connection-config make-rabbitmq-connection-config
  rabbitmq-connection-config?
  (hosts       rabbitmq-connection-config-hosts
               (default '("localhost")))
  (vhost       rabbitmq-connection-config-vhost
               (default "/"))
  (user        rabbitmq-connection-config-user)
  (password    rabbitmq-connection-config-password))

(define rabbitmqctl
  (lambda args
    #~(let ((command (cons*
                      (string-append #$rabbitmq "/sbin/rabbitmqctl")
                      (list #$@args))))
        ;; Set the correct value for the .erlang-cookie
        (copy-file "/var/lib/rabbitmq/.erlang.cookie" "/root/.erlang.cookie")

        (simple-format #t "Running ~A\n" (string-join command))
        (let ((exit-val
               (status:exit-val (apply system* command))))
          (if (zero? exit-val)
              #t
              (begin
                (simple-format #t "Command failed (~A)\n" exit-val)
                #f))))))

(define rabbitmqctl-list-users
  #~(lambda ()
      (use-modules (ice-9 popen)
                   (ice-9 rdelim)
                   (ice-9 regex))
      ;; Set the correct value for the .erlang-cookie
      (copy-file "/var/lib/rabbitmq/.erlang.cookie" "/root/.erlang.cookie")

      (let* ((command `(,(string-append #$rabbitmq "/sbin/rabbitmqctl")
                        "list_users" "-s"))
             (p (apply open-pipe* OPEN_READ command))
             (lines (let loop ((lines '())
                               (line (read-line p)))
                      (if (eof-object? line)
                          (reverse lines)
                          (loop (cons line lines)
                                (read-line p))))))
        (and (let ((status (status:exit-val (close-pipe p))))
               (if (zero? status)
                   #t
                   (begin
                     (simple-format #t
                                    "command: ~A\n"
                                    (string-join command))
                     (error "listing users failed, status ~A" status))))
             (map (lambda (line)
                    (let ((m (string-match "^(\\S+)\\s+\\[(.*?)\\]$" line)))
                      (cons (match:substring m 1)
                            (string-split (match:substring m 2)
                                          #\,))))
                  lines)))))

(define (rabbitmq-create-user-for-connection-config connection-config)
  (match connection-config
    (($ <rabbitmq-connection-config> hosts vhost user password)
     #~(lambda ()
         (let* ((users (#$rabbitmqctl-list-users)))
           (simple-format #t "users ~A\n\n" users)

           (and (if (member #$user (map car users))
                    (begin
                      (simple-format #t "RabbitMQ user ~A already exists\n" #$user)
                      #t)
                    #$(rabbitmqctl "add_user" user password))
                #$(rabbitmqctl "set_permissions" user ".*" ".*" ".*")))))))
