(defun rovanion-scheme-hook ()
  "Custom behaviours for `scheme-mode'."
  (setq-local indent-tabs-mode nil)
  (require 'geiser-guile)
  (add-to-list 'geiser-guile-load-path "~/source/guix/main/")
  (add-to-list 'yas-snippet-dirs       "~/source/guix/main/etc/snippets"))

(add-hook 'scheme-mode-hook 'rovanion-scheme-hook)

(provide 'scheme-conf)
;;; scheme-conf.el ends here
