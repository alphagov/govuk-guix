(define-module (gds services govuk signon)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gds services)
  #:export (<signon-application>
            signon-application
            signon-application?
            signon-application-name
            signon-application-description
            signon-application-redirect-uri
            signon-application-home-uri
            signon-application-uid
            signon-application-secret

            <signon-user>
            signon-user
            signon-user?
            signon-user-name
            signon-user-email
            signon-user-passphrase
            signon-user-application-permissions

            use-real-gds-sso-strategy
            signon-setup-users-script))

(define-record-type* <signon-application>
  signon-application make-signon-application
  signon-application?
  (name signon-application-name)
  (description signon-application-description
               (default ""))
  (redirect-uri signon-application-redirect-uri
                (default #f))
  (home-uri signon-application-name
            (default #f))
  (uid signon-application-name
       (default #f))
  (secret signon-application-name
          (default #f)))

(define-record-type* <signon-user>
  signon-user make-signon-user
  signon-user?
  (name signon-user-name)
  (email signon-user-email)
  (passphrase signon-user-passphrase)
  (role signon-user-role)
  (application-permissions signon-user-application-permissions
                           (default '())))

(define (use-real-gds-sso-strategy services)
  (map
   (lambda (s)
     (service
      (service-kind s)
      (if
       (list? (service-parameters s))
       (map
        (lambda (parameter)
          (if
           (service-startup-config? parameter)
           (service-startup-config-with-additional-environment-variables
            parameter
            '(("GDS_SSO_STRATEGY" . "real")))
           parameter))
        (service-parameters s))
       (service-parameters s))))
   services))

(define (signon-setup-users-script signon-users)
  (plain-file
   "signon-setup-users.rb"
   (string-join
    `("users = ["
      ,(string-join
        (map
         (lambda (user)
           (define sq (cut string-append "'" <> "'"))

           (string-append
            "["
            (string-join
             (list
              (sq (signon-user-name user))
              (sq (signon-user-email user))
              (sq (signon-user-passphrase user))
              (sq (signon-user-role user))
              (string-append
               "["
               (string-join
                (map
                 (match-lambda
                   ((application . permissions)
                    (string-append
                     "[ '" application "', ["
                     (string-join (map sq permissions) ", ")
                     "]]")))
                 (signon-user-application-permissions user)))
               "]"))
             ", ")
            "]"))
         signon-users)
       ",\n")
      "]"
      "
puts \"#{users.length} users to create\"
users.each do |name, email, passphrase, role, application_permissions|
  puts \"Creating #{name}\"
  u = User.new(name: name, email: email)
  u.password = passphrase
  u.role = role

  u.skip_invitation = true
  u.skip_confirmation!

  u.save!
end")
    "\n")))
