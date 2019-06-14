;;;org-conf -- Personal configuration for Org-mode.

;;; Code:

(require 'org-install)

;; Active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R       . t)
   (js      . t)
   (shell   . t)
   (ruby    . t)
   (clojure . t)))

;; Never prompt whether or not to run code
(setq org-confirm-babel-evaluate (lambda (lang body) nil))

(provide 'org-conf)
