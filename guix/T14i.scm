(use-modules (ice-9 popen)
             (ice-9 textual-ports) ; for get-string-all
             (srfi srfi-1)
             (gnu)
             ((nongnu packages linux) :prefix nongnu:)
             (nongnu system linux-initrd))

(use-package-modules
 linux          ; Kernel
 cups           ; Printing
 bash           ; Shell
 security-token ; Yubikey.
 xdisorg        ; Physlock.
 )
(use-service-modules
 desktop
 networking
 ssh
 xorg
 pm
 cups
 avahi
 sound
 )


(define package-list
  (let* ((package-lists-stdout (open-pipe* OPEN_READ "/home/rovanion/source/.files/package-lists.sh" "workstation"))
        (package-list (string-tokenize (get-string-all package-lists-stdout))))
    (close-pipe package-lists-stdout)
    package-list))


(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

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
  (locale "sv_SE.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout (keyboard-layout "se" "dvorak"))
  (kernel nongnu:linux)
  (initrd microcode-initrd)
  (firmware (list nongnu:linux-firmware nongnu:sof-firmware))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout keyboard-layout)))
  (mapped-devices
    (list (mapped-device
            (source
              (uuid "2d3a56c2-897a-4e7c-9b1b-5bbf390f22bb"))
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
             (device (uuid "C8DE-2EEF" 'fat32))
             (type "vfat"))
           %base-file-systems))
  (swap-devices
   (list (swap-space (target (uuid "a28e535d-694e-444a-8dc2-3a4ca0397576"))
                     (dependencies mapped-devices))))
  (host-name "T14i")
  (name-service-switch %mdns-host-lookup-nss)
  (users (cons* (user-account
                  (name "rovanion")
                  (comment "Rovanion Luckey")
                  (group "users")
                  (home-directory "/home/rovanion")
                  (supplementary-groups
                   '("wheel"  ; Root access via sudo.
                     "netdev" ; Manage network devices.
                     "audio"  ; Sound card access.
                     "video"  ; Webcam access.
                     "plugdev" ; For yubikey.
                     ))) ;  "kvm" "libvirt"
                %base-user-accounts))
  (packages
    (append
     (map (compose list specification->package+output) package-list)
      %base-packages))
  (services
   (cons*
    (service openssh-service-type)
    (service slim-service-type
             (slim-configuration
              (xorg-configuration
               (xorg-configuration
                (keyboard-layout keyboard-layout)
                (extra-config (list %xorg-libinput-config))))))
    (service cups-service-type
             (cups-configuration
              (web-interface? #t)
              (extensions
               (list cups-filters epson-inkjet-printer-escpr hplip-minimal splix))))
    (service screen-locker-service-type
             (screen-locker-configuration
              (name "physlock")
              (program (file-append physlock "/bin/physlock"))))
    (simple-service 'block-reddit hosts-service-type
                    (list (host "127.0.0.1" "www.reddit.com")
                          (host "127.0.0.1" "old.reddit.com")))
    (service block-facebook-hosts-service-type)
    (extra-special-file "/usr/bin/env"
                        (file-append coreutils "/bin/env"))
    (extra-special-file "/bin/bash"
                        (file-append bash "/bin/bash"))
    (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
    (remove (lambda (service)
              (eq? (service-kind service) gdm-service-type))
            nonguix-desktop-services)))
  )
