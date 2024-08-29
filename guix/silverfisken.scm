(define-module (base-system)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (gnu system nss)
  #:use-module (gnu services pm)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-service-modules nix)
(use-service-modules desktop xorg)
(use-package-modules certs)
(use-package-modules shells)

;; Allow members of the "video" group to change the screen brightness.
(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

;; Override the default =%desktop-services= to add the =udev= backlight configuration.
(define %my-desktop-services
  (modify-services %desktop-services
                   (udev-service-type config =>
                                      (udev-configuration (inherit config)
                                                          (rules (cons %backlight-udev-rule
                                                                       (udev-configuration-rules config)))))))

;; Use the =libinput= driver for all input devices since it's a bit more modern than the default.
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

(define-public base-operating-system
  (operating-system
    (host-name "silverfisken")
    (timezone "Europe/Stockholm")
    (locale "sv_SE.utf8")

    ;; Use non-free Linux and firmware, wiith patched microcode, livin wild!
    (kernel linux)
    (firmware (list linux-firmware))
    (initrd microcode-initrd)

    ;; Choose US English keyboard layout.  The "altgr-intl"
    ;; variant provides dead keys for accented characters.
    (keyboard-layout (keyboard-layout "se" "dvorak"))

    ;; Use the UEFI variant of GRUB with the EFI System
    ;; Partition mounted on /boot/efi.
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")
                 (keyboard-layout keyboard-layout)))


    ;; Guix doesn't like it when there isn't a file-systems
    ;; entry, so add one that is meant to be overridden
    (file-systems (cons*
                   (file-system
                     (mount-point "/tmp")
                     (device "none")
                     (type "tmpfs")
                     (check? #f))
                   %base-file-systems))

    (users (cons (user-account
                  (name "rovanion")
                  (comment "Rovanion Luckey")
                  (group "users")
                  (home-directory "/home/rovanion")
                  (supplementary-groups '(
                                          "wheel"     ;; sudo
                                          "netdev"    ;; network devices
                                          "kvm"
                                          "tty"
                                          "input"
                                          "docker"
                                          "realtime"  ;; Enable realtime scheduling
                                          "lp"        ;; control bluetooth devices
                                          "audio"     ;; control audio devices
                                          "video")))  ;; control video devices

                 %base-user-accounts))

    ;; Add the 'realtime' group
    (groups (cons (user-group (system? #t) (name "realtime"))
                  %base-groups))

    ;; Install bare-minimum system packages
    (packages (append (list
                        git
                        ntfs-3g
                        exfat-utils
                        fuse-exfat
                        vim
                        emacs
                        rxvt-unicode
                        bluez
                        bluez-alsa
                        pipewire
			awesome
			xkbcomp             ;; Keymap compiler for configure-mouse-and-keyboard.
			arandr              ;; Set resolution and screen composition.
			htop                ;; Process monitor.
			firefox             ;; Web browser.
			keepassxc           ;; Password manager.
                        xf86-input-libinput
                        nss-certs     ;; for HTTPS access
                        gvfs)         ;; for user mounts
                    %base-packages))

    ;; Use the "desktop" services, which include the X11 log-in service,
    ;; networking with NetworkManager, and more
    (services (cons* (service slim-service-type
                              (slim-configuration
                                (xorg-configuration
                                  (xorg-configuration
                                    (keyboard-layout keyboard-layout)
                                    (extra-config (list %xorg-libinput-config))))))
                    (pam-limits-service ;; This enables JACK to enter realtime mode
                     (list
                      (pam-limits-entry "@realtime" 'both 'rtprio 99)
                      (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))
                    (extra-special-file "/usr/bin/env"
                      (file-append coreutils "/bin/env"))
                    (service thermald-service-type)
                    (service docker-service-type)
                    (service libvirt-service-type
                             (libvirt-configuration
                              (unix-sock-group "libvirt")
                              (tls-port "16555")))
                    (service cups-service-type
                             (cups-configuration
                               (web-interface? #t)
                               (extensions
                                 (list cups-filters))))
                    (service nix-service-type)
                    (bluetooth-service #:auto-enable? #t)
                    (remove (lambda (service)
                                (eq? (service-kind service) gdm-service-type))
                            %my-desktop-services)))

    ;; Allow resolution of '.local' host names with mDNS
    (name-service-switch %mdns-host-lookup-nss)))


(operating-system
 (inherit base-operating-system)
  (file-systems (append
                 (list (file-system
                         (device (uuid "17e8088b-4af1-43e7-9e5f-f272f6ab4b25"))
                         (mount-point "/")
                         (type "ext4"))
                       (file-system
                         (device (uuid "D653-845A" 'fat))
                         (mount-point "/boot/efi")
                         (type "vfat"))
		       (file-system
                         (device (uuid "f002b847-d9ee-4a19-a5df-de2f6fa8f4ec"))
                         (mount-point "/home")
                         (type "ext4")))
                 %base-file-systems)))
