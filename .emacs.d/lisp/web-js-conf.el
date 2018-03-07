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
(add-to-list 'auto-mode-alist '("\\.tsx\\'"        . web-mode))

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)

(defun pretty-print-json (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "python -m json.tool" (current-buffer) t))

;; Start tide-mode when editing a typescript tsx file.
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(fset 'align-javascript-object-literal
   "\C-u\C-[xalig\C-i-r\C-i\C-m\C-a:\C-m\C-m\C-mn")

(require 'flycheck)
;; Enable typescript-tslint checker.
(flycheck-add-mode 'typescript-tslint 'web-mode)

(provide 'web-js-conf)
;;; web-mode-conf.el ends here
