(define-module (gds services utils databases elasticsearch)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:export (<elasticsearch-connection-config>
            elasticsearch-connection-config
            elasticsearch-connection-config?
            elasticsearch-connection-config-port
            elasticsearch-connection-config-host

            elasticsearch-restore-gexp))

(define-record-type* <elasticsearch-connection-config>
  elasticsearch-connection-config make-elasticsearch-connection-config
  elasticsearch-connection-config?
  (host elasticsearch-connection-config-host
        (default "localhost"))
  (port elasticsearch-connection-config-port
        (default 5432)))

(define* (elasticsearch-restore-gexp database-connection index-name file
                                     #:key alias overrides batch-size)
  (match database-connection
    (($ <elasticsearch-connection-config> host port)
     #~(lambda ()
         (let ((command
                (list
                 (string-append #$es-dump-restore "/bin/es_dump_restore")
                 #$(if alias "restore_alias" "restore")
                 #$(simple-format #f "http://~A:~A" host (number->string port))
                 #$@(if alias (list alias) '())
                 #$index-name
                 #$file
                 #$@(if overrides (list overrides) '())
                 #$@(if batch-size (list (number->string batch-size)) '()))))
           (simple-format #t "Running command: ~A\n" (string-join command " "))
           (zero?
            (apply system* command)))))))
