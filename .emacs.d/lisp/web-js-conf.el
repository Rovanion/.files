;;; Code.

(require 'js2-mode)

;; make js2-mode the mode for javascript files
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


(require 'less-css-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"        . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'"        . web-mode))

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)

(defun pretty-print-json (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "python -m json.tool" (current-buffer) t))

(provide 'web-js-conf)
;;; web-mode-conf.el ends here
