(define-module (gds systems utils packer)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:export (packer-template-for-disk-image
            packer-template-for-govuk-system-init

            packer-build-template-script))

(define-record-type* <packer-template>
  packer-template make-packer-template
  packer-template?
  (builders           packer-template-builders)
  (description        packer-template-description
                      (default #f))
  (min-packer-version packer-template-min-packer-version
                      (default #f))
  (post-processors    packer-template-post-processors
                      (default '()))
  (provisioners       packer-template-provisioners
                      (default '()))
  (variables          packer-template-variables
                      (default '())))

(define-record-type* <block-device-mapping>
  block-device-mapping make-block-device-mapping
  block-device-mapping?
  (delete-on-termination? block-device-mapping-delete-on-termination?
                          (default #f))
  (device-name            block-defice-mapping-device-name)
  (encrypted?             block-device-mapping-encrypted?
                          (default 'unset))
  (kms-key-id             block-device-mapping-kms-key-id
                          (default 'unset))
  (iops                   block-device-mapping-iops
                          (default 'unset))
  (no-device              block-device-mapping-no-device
                          (default 'unset))
  (snapshot-id            block-device-mapping-snapshot-id
                          (default 'unset))
  (virtual-name           block-device-mapping-virtual-name
                          (default 'unset))
  (volume-size            block-device-mapping-volume-size
                          (default 'unset))
  (volume-type            block-device-mapping-volume-type))

(define block-device-mapping->alist-gexp
  (match-lambda
    (($ <block-device-mapping> delete-on-termination? device-name encrypted?
                               kms-key-id iops no-device snapshot-id virtual-name
                               volume-size volume-type)
     #~(list
        #$@(filter-map
            (match-lambda
              ((key . value)
               (if (not (eq? value 'unset))
                   #~(cons '#$key #$value)
                   #f)))
            `((delete_on_termination . ,delete-on-termination?)
              (device_name           . ,device-name)
              (encrypted             . ,encrypted?)
              (kms_key_id            . ,kms-key-id)
              (iops                  . ,iops)
              (no_device             . ,no-device)
              (snapshot_id           . ,snapshot-id)
              (virtual_name          . ,virtual-name)
              (volume_size           . ,volume-size)
              (volume_type           . ,volume-type)))))))

(define-record-type* <packer-amazon-chroot-builder>
  packer-amazon-chroot-builder make-packer-amazon-chroot-builder
  packer-amazon-chroot-builder?
  (access-key                packer-amazon-chroot-builder-access-key)
  (secret-key                packer-amazon-chroot-builder-access-key)
  (region                    packer-amazon-chroot-builder-region)
  (ami-name                  packer-amazon-chroot-builder-ami-name)
  (source-ami                packer-amazon-chroot-builder-source-ami)
  (ami-description           packer-amazon-chroot-builder-ami-description
                             (default #f))
  (ami-virtualization-type   packer-amazon-chroot-builder-ami-virtualization-type
                             (default "paravirtual"))
  (chroot-mounts             packer-amazon-chroot-builder-ami-virtualization-chroot-mounts
                             (default #f))
  (from-scratch?             packer-amazon-chroot-builder-from-scratch?
                             (default #f))
  (pre-mount-commands        packer-amazon-chroot-builder-pre-mount-commands
                             (default '()))
  (post-mount-commands       packer-amazon-chroot-builder-post-mount-commands
                             (default '()))
  (root-volume-size          packer-amazon-chroot-builder-root-volume-size
                             (default #f))
  (root-device-name          packer-amazon-chroot-builder-root-device-name
                             (default #f))
  (ami-block-device-mappings packer-amazon-chroot-builder-ami-block-device-mappings
                             (default #f))
  (additional-options        packer-amazon-chroot-builder-additional-options
                             (default '())))

(define packer-amazon-chroot-builder->alist-gexp
  (match-lambda
    (($ <packer-amazon-chroot-builder> access-key secret-key region
                                       ami-name source-ami ami-description
                                       ami-virtualization-type
                                       chroot-mounts from-scratch?
                                       pre-mount-commands post-mount-commands
                                       root-volume-size root-device-name
                                       ami-block-device-mappings
                                       additional-options)
     #~`((type . "amazon-chroot")
         (access_key . ,#$access-key)
         (secret_key . ,#$secret-key)
         (region . ,#$region)
         (ami_name . ,#$ami-name)
         ,@(if #$source-ami
               `((source_ami . ,#$source-ami))
               '())
         ,@(if #$ami-description
               `((ami_description . ,#$ami-description))
               '())
         (ami_virtualization_type . ,#$ami-virtualization-type)
         ,@(if #$chroot-mounts
               `((chroot_mounts . ,#$chroot-mounts))
               '())
         ,@(if #$from-scratch?
               '((from_scratch . #t))
               '())
         #$@(if pre-mount-commands
                #~((pre_mount_commands . ,(list->vector
                                           (list #$@pre-mount-commands))))
                #~())
         #$@(if post-mount-commands
                #~((post_mount_commands . ,(list->vector
                                            (list #$@post-mount-commands))))
                #~())
         #$@(if root-volume-size
                #~((root_volume_size . ,#$root-volume-size))
                #~())
         #$@(if root-device-name
                #~((root_device_name . ,#$root-device-name))
                #~())
         #$@(if ami-block-device-mappings
                #~((ami_block_device_mappings
                    . ,(list->vector
                        (list #$@(map block-device-mapping->alist-gexp
                                      ami-block-device-mappings)))))
                #~())
         #$@(map (match-lambda
                   ((key . value)
                    #~(#$key . ,#$value)))
                 additional-options)))))

(define-gexp-compiler (password-file-compiler
                       (record <packer-template>) system target)
  (match record
    (($ <packer-template> builders description min-packer-version post-processors
                          provisioners variables)
     (gexp->derivation
      "packer-template.json"
      (with-extensions (list guile-json-3)
        #~(begin
            (use-modules (srfi srfi-1)
                         (ice-9 match)
                         (json))
            (call-with-output-file #$output
              (lambda (port)
                (display
                 (scm->json-string
                  `(#$@(if min-packer-version
                           #~((min_packer_version . ,#$min-packer-version))
                           #~())
                    #$@(if description
                           #~((description . ,#$description))
                           #~())
                    (variables . #$variables)
                    (builders . ,(list->vector
                                  (list #$@(map packer-amazon-chroot-builder->alist-gexp
                                                builders))))
                    (provisioners . ,(list->vector
                                      (list #$@provisioners)))
                    (post-processors . ,(list->vector
                                         (list #$@post-processors))))
                  #:pretty #t)
                 port)
                (display "\n" port)))))
      #:local-build? #t))))

(define (packer-build-template-script template)
  (gexp->script
   "build-packer-template"
   #~(execl
      #$(file-append packer "/bin/packer")
      "packer" ;; This argument is used at the name of the script
      "build"
      #$template)))

(define (builder-for-disk-image disk-image)
  (packer-amazon-chroot-builder
   (ami-name "placeholder-ami-name")
   (access-key "{{user `aws_access_key_id`}}")
   (secret-key "{{user `aws_secret_access_key`}}")
   (region "eu-west-1")
   (source-ami #f)
   (from-scratch? #t)
   (root-volume-size 200)
   (root-device-name "xvda")
   (ami-virtualization-type "hvm")
   (ami-block-device-mappings
    (list (block-device-mapping
           (device-name "xvda")
           (delete-on-termination? #t)
           (volume-type "gp2"))))
   (pre-mount-commands
    (list #~(string-append
             #$(file-append coreutils "/bin/dd")
             " if=" #$disk-image
             " of={{.Device}}"
             " bs=1M")
          #~(string-append
             #$(file-append parted "/sbin/parted")
             " -s {{.Device}} rm 2")
          #~(string-append
             #$(file-append cloud-utils "/bin/growpart")
             " {{.Device}} 1")
          #~(string-append
             #$(file-append e2fsprogs "/sbin/e2fsck")
             " -p -v -f {{.Device}}1")
          #~(string-append
             #$(file-append e2fsprogs "/sbin/resize2fs")
             " {{.Device}}1")))
   (additional-options
    '((ena_support . #t)))))

(define (packer-template-for-disk-image disk-image)
  (packer-template
   (variables
    '((aws_access_key_id . "{{ env `AWS_ACCESS_KEY_ID` }}")
      (aws_secret_access_key . "{{ env `AWS_SECRET_ACCESS_KEY` }}")))
   (builders
    (list (builder-for-disk-image disk-image)))))

(define (builder-for-govuk-system-init ami-name args data-snapshot)
  (packer-amazon-chroot-builder
   (ami-name ami-name)
   (access-key "{{user `aws_access_key_id`}}")
   (secret-key "{{user `aws_secret_access_key`}}")
   (region "eu-west-1")
   (source-ami #f)
   (from-scratch? #t)
   (root-volume-size 200)
   (root-device-name "xvda")
   (ami-virtualization-type "hvm")
   (ami-block-device-mappings
    (list (block-device-mapping
           (device-name "xvda")
           (delete-on-termination? #t)
           (volume-type "gp2"))))
   (pre-mount-commands
    (list #~(string-append
             #$(file-append parted "/sbin/parted")
             " "
             #$(string-join
                '("--align=optimal"
                  "--script"
                  "{{.Device}}"
                  "mklabel msdos"
                  "--"
                  "mkpart primary ext4 1MiB 100%"
                  "set 1 boot on")))
          #~(string-append
             #$(file-append e2fsprogs "/sbin/mkfs.ext4")
             " {{.Device}}1")
          #~(string-append
             #$(file-append e2fsprogs "/sbin/e2label")
             " {{.Device}}1 my-root")))
   (post-mount-commands
    `(,(string-append
        (getenv "GOVUK_GUIX_ROOT")
        "/bin/govuk system init --target={{.MountPath}} "
        (string-join (map (lambda (arg)
                            (string-append "'" arg "'"))
                          args)
                     " "))
      ,@(if data-snapshot
            (let ((snapshot-var-lib
                   (string-append data-snapshot
                                  "/var/lib")))
              (cons
               "mkdir -p {{.MountPath}}/var/lib"
               (filter-map
                (lambda (archive-name)
                  #~(string-join
                     (list
                      #$(file-append tar "/bin/tar")
                      "--directory={{.MountPath}}/var/lib"
                      (string-append "--use-compress-program="
                                     #$(file-append pigz "/bin/pigz"))
                      "--checkpoint=1000"
                      "--checkpoint-action=echo='%ds: %{read,wrote}T'"
                      "--extract"
                      "--file" #$(string-append snapshot-var-lib
                                                "/" archive-name))
                     " "))
                (scandir snapshot-var-lib
                         (negate
                          (lambda (f)
                            (member f '("." ".."))))))))
            '())))
   (additional-options
    '((ena_support . #t)))))

(define* (packer-template-for-govuk-system-init ami-name args #:key data-snapshot)
  (packer-template
   (variables
    '((aws_access_key_id . "{{ env `AWS_ACCESS_KEY_ID` }}")
      (aws_secret_access_key . "{{ env `AWS_SECRET_ACCESS_KEY` }}")))
   (builders
    (list (builder-for-govuk-system-init ami-name args data-snapshot)))))
