;;;org-conf -- Personal configuration for Org-mode.

;;; Code:


(require 'org)
;; To enable the easy templates.
(require 'org-tempo)
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(require 'cider)

;; Active Babel languages.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R       . t)
   (js      . t)
   (shell   . t)
   (ruby    . t)
   (clojure . t)))

;; Never prompt whether or not to run code.
(setq org-confirm-babel-evaluate (lambda (lang body) nil))

(provide 'org-conf)
