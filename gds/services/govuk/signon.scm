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

            <signon-api-user>
            signon-api-user
            signon-api-user?
            signon-api-user-name
            signon-api-user-email
            signon-api-user-authorisation-permissions

            <signon-authorisation>
            signon-authorisation
            signon-authorisation?
            signon-authorisation-application-name
            signon-authorisation-token

            use-gds-sso-strategy
            update-signon-application-with-random-oauth
            update-signon-api-user-with-random-authorisation-tokens
            signon-setup-users-script
            signon-setup-api-users-script
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

(define-record-type* <signon-api-user>
  signon-api-user make-signon-api-user
  signon-api-user?
  (name signon-api-user-name)
  (email signon-api-user-email)
  (authorisation-permissions signon-api-user-authorisation-permissions
                             (default '())))

(define-record-type* <signon-authorisation>
  signon-authorisation make-signon-authorisation
  signon-authorisation?
  (application-name signon-authorisation-application-name)
  (token signon-authorisation-token
         (default #f)))

(define (update-signon-application-with-random-oauth app)
  (signon-application
   (inherit app)
   (oauth-id (random-base16-string 64))
   (oauth-secret (random-base16-string 64))))

(define (update-signon-authorisation-with-random-token authorisation)
  (signon-authorisation
   (inherit authorisation)
   (token (random-base16-string 30))))

(define (update-signon-api-user-with-random-authorisation-tokens api-user)
  (signon-api-user
   (inherit api-user)
   (authorisation-permissions
    (map
     (match-lambda
       ((authorisation . permissions)
        (cons
         (update-signon-authorisation-with-random-token authorisation)
         permissions)))
     (signon-api-user-authorisation-permissions api-user)))))

(define (use-gds-sso-strategy services strategy)
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
            `(("GDS_SSO_STRATEGY" . ,strategy)))
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
                 (signon-user-application-permissions user))
                ", ")
               "]"))
             ", ")
            "]"))
         signon-users)
       ",\n")
      "]"
      "
puts \"#{users.length} users to create\"

Devise.deny_old_passwords = false

users.each do |name, email, passphrase, role, application_permissions|
  puts \"Creating #{name}\"
  u = User.where(name: name, email: email).first_or_initialize
  u.password = passphrase
  u.role = role

  u.skip_invitation = true
  u.skip_confirmation!

  u.save!

  application_permissions.each do |application_name, permissions|
    app = Doorkeeper::Application.find_by_name!(application_name)
    permissions.each do |permission|
      u.grant_application_permission(app, permission)
    end
  end
end")
    "\n")))

(define (signon-setup-api-users-script signon-api-users)
  (plain-file
   "signon-setup-api-users.rb"
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
              (sq (signon-api-user-name user))
              (sq (signon-api-user-email user))
              (string-append
               "["
               (string-join
                (map
                 (match-lambda
                   ((($ <signon-authorisation> application-name token)
                     .
                     permissions)
                    (string-append
                     "[ '" application-name "', '" token "', ["
                     (string-join (map sq permissions) ", ")
                     "]]")))
                 (signon-api-user-authorisation-permissions user)))
               "]"))
             ", ")
            "]"))
         signon-api-users)
       ",\n")
      "]"
      "
puts \"#{users.length} api users to create\"

users.each do |name, email, authorisation_permissions|
  puts \"Creating #{name}\"

  passphrase = SecureRandom.urlsafe_base64

  u = ApiUser.where(email: email).first_or_initialize(
    name: name,
    password: passphrase,
    password_confirmation: passphrase
  )
  u.api_user = true
  u.skip_confirmation!
  u.save!

  authorisation_permissions.each do |application_name, token, permissions|
    app = Doorkeeper::Application.find_by_name!(application_name)

    permissions.each do |permission|
      u.grant_application_permission(app, permission)
    end

    authorisation = u.authorisations.build(
      expires_in: ApiUser::DEFAULT_TOKEN_LIFE
    )
    authorisation.application_id = app.id
    authorisation.save!

    authorisation.token = token
    authorisation.save!
  end
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
puts \"#{apps.length} applications to create\"

apps.each do |name, description, redirect_uri, home_uri, supported_permissions, oauth_id, oauth_secret|
  puts \"Creating #{name}\"

  app = Doorkeeper::Application.where(name: name).first_or_create!(
    redirect_uri: redirect_uri,
    description: description,
    home_uri: home_uri,
    uid: oauth_id,
    secret: oauth_secret
  )

  supported_permissions.each do |permission|
    SupportedPermission.where(
      name: permission,
      application_id: app.id
    ).first_or_create!
  end
end")
    "\n")))
