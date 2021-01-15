(add-hook 'scheme-mode-hook 'rovanion-scheme-hook)

(defun rovanion-scheme-hook ()
  "Custom behaviours for `scheme-mode'."
  (setq-local indent-tabs-mode nil))

(provide 'scheme-conf)
;;; scheme-conf.el ends here
