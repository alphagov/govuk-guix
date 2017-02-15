(define-module (gds services govuk signon)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gds services)
  #:use-module (gds services utils)
  #:export (<signon-application>
            signon-application
            signon-application?
            signon-application-name
            signon-application-description
            signon-application-redirect-uri
            signon-application-home-uri
            signon-application-supported-permissions
            signon-application-oauth-id
            signon-application-oauth-secret

            <signon-user>
            signon-user
            signon-user?
            signon-user-name
            signon-user-email
            signon-user-passphrase
            signon-user-application-permissions

            use-real-gds-sso-strategy
            update-signon-application-with-random-oauth
            signon-setup-users-script
            signon-setup-applications-script))

(define-record-type* <signon-application>
  signon-application make-signon-application
  signon-application?
  (name signon-application-name)
  (description signon-application-description
               (default ""))
  (redirect-uri signon-application-redirect-uri
                (default #f))
  (home-uri signon-application-home-uri
            (default #f))
  (supported-permissions signon-application-supported-permissions
                         (default '()))
  (oauth-id signon-application-oauth-id
            (default #f))
  (oauth-secret signon-application-oauth-secret
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

(define (update-signon-application-with-random-oauth app)
  (signon-application
   (inherit app)
   (oauth-id (random-base16-string 64))
   (oauth-secret (random-base16-string 64))))

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

(define (signon-setup-applications-script signon-applications)
  (plain-file
   "signon-setup-applications.rb"
   (string-join
    `("apps = ["
      ,(string-join
        (map
         (lambda (app)
           (define sq (cut string-append "'" <> "'"))

           (string-append
            "["
            (string-join
             (list
              (sq (signon-application-name app))
              (sq (signon-application-description app))
              (sq (signon-application-redirect-uri app))
              (sq (signon-application-home-uri app))
              (string-append
               "["
               (string-join
                (map sq (signon-application-supported-permissions app))
                ", ")
              "]")
              (sq (signon-application-oauth-id app))
              (sq (signon-application-oauth-secret app)))
             ", ")
            "]"))
         signon-applications)
        ",\n")
      "]"
      "
puts \"#{apps.length} applicationsn to create\"
apps.each do |name, description, redirect_uri, home_uri, supported_permissions, oauth_id, oauth_secret|
  puts \"Creating #{name}\"

  app = Doorkeeper::Application.create!(
    name: name,
    redirect_uri: redirect_uri,
    description: description,
    home_uri: home_uri,
    uid: oauth_id,
    secret: oauth_secret
  )

  supported_permissions.each do |permission|
    SupportedPermission.create(application_id: app.id, name: permission)
  end
end")
    "\n")))
