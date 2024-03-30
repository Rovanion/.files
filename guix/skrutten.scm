;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.

(use-modules (gnu))
(use-service-modules cups desktop networking ssh xorg)
;; Import nonfree linux module.
;; (use-modules (nongnu packages linux)    
;;              (nongnu system linux-initrd))

(operating-system
  ;; Use non-free Linux and firmware.
  ;; (kernel linux)
  ;; (initrd microcode-initrd)
  ;; (firmware (list linux-firmware))

  (locale "sv_SE.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout (keyboard-layout "se" "dvorak"))
  (host-name "skrutten")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "rovanion")
                  (comment "Rovanion Luckey")
                  (group "users")
                  (home-directory "/home/rovanion")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (specification->package "nss-certs"))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list

                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 (service openssh-service-type)
                 (service dhcp-client-service-type)
                 (service ntp-service-type)
                 (service gpm-service-type))

           ;; This is the default list of services we
           ;; are appending to.
           %base-services))
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sdb"))
                (keyboard-layout keyboard-layout)))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "bf03a82f-7180-46f6-b870-dece0bcea402"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
