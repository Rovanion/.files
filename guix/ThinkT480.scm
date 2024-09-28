;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             ; (gnu services virtualization)
             (gnu services databases)
             (gnu packages cups)
             (gnu packages databases)
	     (gnu packages linux)
             ((nongnu packages linux) :prefix nongnu:)
             (nongnu system linux-initrd))
(use-service-modules desktop networking ssh xorg pm cups avahi)

(define nonguix-desktop-services
  (modify-services %desktop-services
    (guix-service-type config =>
                       (guix-configuration
                        (inherit config)
                        (substitute-urls
                         (append (list "https://substitutes.nonguix.org")
                                 %default-substitute-urls))
                        (authorized-keys
                         (append (list (plain-file "nonguix.pub"
                                                   "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                                 %default-authorized-guix-keys))))))

(operating-system
  (locale "en_GB.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout (keyboard-layout "se" "dvorak"))
  (kernel nongnu:linux-lts)
  (initrd microcode-initrd)
  (firmware (list nongnu:linux-firmware))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout keyboard-layout)))
  (mapped-devices
    (list (mapped-device
            (source
              (uuid "a6abdff8-34ff-4eb7-8aaf-426e3c8769c0"))
            (target "guix-root")
            (type luks-device-mapping))))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device "/dev/mapper/guix-root")
             (type "ext4")
             (dependencies mapped-devices))
           (file-system
             (mount-point "/boot/efi")
             (device (uuid "960F-1606" 'fat32))
             (type "vfat"))
           %base-file-systems))
  (host-name "ThinkT480")
  (name-service-switch %mdns-host-lookup-nss)
  (users (cons* (user-account
                  (name "rovanion")
                  (comment "Rovanion Luckey")
                  (group "users")
                  (home-directory "/home/rovanion")
                  (supplementary-groups
                   '("wheel" "netdev" "audio" "video"))) ;  "kvm" "libvirt"
                %base-user-accounts))
  (packages
    (append
     (map specification->package
          '("awesome"
            "nss-certs"
            "gvfs"
            "cups"
            "font-wqy-zenhei"
            "font-wqy-microhei"
            "font-adobe-source-han-sans"
            "papirus-icon-theme"
            "adwaita-icon-theme"
            "hicolor-icon-theme" ; Only installed to remove the red wrong way roadmarker icon from nm-applet.
            ))
      %base-packages))
  (services
    (append
      (list (service openssh-service-type)
            (set-xorg-configuration
             (xorg-configuration
              (keyboard-layout keyboard-layout)))
            (service cups-service-type
                     (cups-configuration
                      (web-interface? #t)
                      (extensions
                       (list cups-filters epson-inkjet-printer-escpr hplip-minimal splix))))
            ;; (service libvirt-service-type
            ;;           (libvirt-configuration
            ;;            (unix-sock-group "libvirt")
            ;;            (tls-port "16555")))
            (service postgresql-service-type
                     (postgresql-configuration
                      (postgresql postgresql-13))))
      nonguix-desktop-services))
  (hosts-file
   (plain-file "hosts"
               (string-append (local-host-aliases host-name)
                              %facebook-host-aliases
                              "#127.0.0.1 www.reddit.com"))))
