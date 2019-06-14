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

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("sh"
                      "bash"
                      "shell"
                      "clojure"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(provide 'org-conf)
