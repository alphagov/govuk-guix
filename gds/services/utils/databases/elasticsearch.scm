(define-module (gds services utils databases elasticsearch)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pv)
  #:export (<elasticsearch-connection-config>
            elasticsearch-connection-config
            elasticsearch-connection-config?
            elasticsearch-connection-config-port
            elasticsearch-connection-config-host))

(define-record-type* <elasticsearch-connection-config>
  elasticsearch-connection-config make-elasticsearch-connection-config
  elasticsearch-connection-config?
  (host elasticsearch-connection-config-host
        (default "localhost"))
  (port elasticsearch-connection-config-port
        (default 5432)))
