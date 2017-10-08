;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;;
;;; This file is not officially part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gds packages third-party mongodb)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public mongodb
  (package
    (name "mongodb")
    (version "3.4.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mongodb/mongo/archive/r"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0gidwyvh3bdwmk2pccgkqkaln4ysgn8iwa7ihjzllsq0rdg95045"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("scons" ,scons)
       ("python" ,python-2)
       ("perl" ,perl)))
    (arguments
     `(#:tests? #f ; There is no 'check' target.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase
         (add-after 'unpack 'scons-propagate-environment
           (lambda _
             ;; Modify the SConstruct file to arrange for
             ;; environment variables to be propagated.
             (substitute* "SConstruct"
               (("^env = Environment\\(")
                "env = Environment(ENV=os.environ, "))))
         (add-after 'unpack 'create-version-file
           (lambda _
             (call-with-output-file "version.json"
               (lambda (port)
                 (display ,(simple-format #f "{
    \"version\": \"~A\"
}" version) port)))))
         (replace 'build
           (lambda _
             (zero? (system* "scons"
                             (format #f "--jobs=~a" (parallel-job-count))
                             "--disable-warnings-as-errors"
                             "mongod" "mongo" "mongos"))))
         (replace 'install
           (lambda _
             (let ((bin  (string-append (assoc-ref %outputs "out") "/bin")))
               (install-file "mongod" bin)
               (install-file "mongos" bin)
               (install-file "mongo" bin)))))))
    (home-page "https://www.mongodb.org/")
    (synopsis "High performance and high availability document database")
    (description "Mongo is a high-performance, high availability,
schema-free document-oriented database.  A key goal of MongoDB is to bridge
the gap between key/value stores (which are fast and highly scalable) and
traditional RDBMS systems (which are deep in functionality).")
    (license (list license:agpl3 license:asl2.0))))

(define-public mongo-tools
  (package
    (name "mongo-tools")
    (version "3.4.9")
    (source
     (origin (method url-fetch)
             (uri (string-append "https://github.com/mongodb/mongo-tools"
                                 "/archive/r" version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1ap0p2051az00zp64j2v2a2yilc0nwajga37fl82qj18lspifvsb"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'install)
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bash (which "bash"))
                   (out (assoc-ref outputs "out")))
               (and
                (zero? (system* bash "set_gopath.sh"))
                (begin
                  (setenv "GOPATH" (string-join
                                    (list
                                     (string-append (getcwd) "/.gopath")
                                     (string-append (getcwd) "/vendor"))
                                    ":"))
                  (mkdir-p (string-append out "/bin"))
                  (setenv "GOBIN" (string-append out "/bin"))
                  #t)
                (let build ((tools
                             '("bsondump" "mongodump" "mongoexport" "mongofiles"
                               "mongoimport" "mongooplog" "mongorestore"
                               "mongostat" "mongotop")))
                  (if (null? tools)
                      #t
                      (if (let* ((tool (car tools))
                                 (command
                                  `("go" "install" "-v"
                                    "-tags=\"ssl sasl\""
                                    "-ldflags"
                                    "-extldflags=-Wl,-z,now,-z,relro"
                                    ,(string-append tool "/main/" tool ".go"))))
                            (simple-format #t "build: running ~A\n"
                                           (string-join command))
                            (zero? (apply system* command)))
                          (build (cdr tools))
                          #f))))))))))
    (native-inputs
     `(("go" ,go)))
    (home-page "https://github.com/mongodb/mongo-tools")
    (synopsis "Various tools for interacting with MongoDB and BSON")
    (description
     "This package includes a collection of tools related to MongoDB.
@table @code
@item bsondump
Display BSON files in a human-readable format
@item mongoimport
Convert data from JSON, TSV or CSV and insert them into a collection
@item mongoexport
Write an existing collection to CSV or JSON format
@item mongodump/mongorestore
Dump MongoDB backups to disk in the BSON format
@item mongorestore
Read MongoDB backups in the BSON format, and restore them to a live database
@item mongostat
Monitor live MongoDB servers, replica sets, or sharded clusters
@item mongofiles
Read, write, delete, or update files in GridFS
@item mongooplog
Replay oplog entries between MongoDB servers
@item mongotop
Monitor read/write activity on a mongo server
@end table")
    (license "expat")))
