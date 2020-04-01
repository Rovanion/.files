;;; nginx-conf -- Personal configuration for nginx conf files.

;;; Code:
(require 'nginx-mode)

(add-to-list 'auto-mode-alist '("\\`/etc/nginx/" . nginx-mode))
(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

;; Please use tabs mr. nginx-mode.
(setq nginx-indent-tabs-mode nil)

(provide 'nginx-conf)
;; Format before save,
