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
   (sh      . t)
   (ruby    . t)
   (shell   . t)
   (python  . t)
   (clojure . t)))

;; Never prompt whether or not to run code.
(setq org-confirm-babel-evaluate (lambda (lang body) nil))

;; Avoid accidentally editing folded regions, say by adding text after an Org “⋯”.
(setq org-catch-invisible-edits 'show)

;; Cache results by default.
(setq org-babel-default-header-args
      (cons '(:cache . "yes")
            (assq-delete-all :cache org-babel-default-header-args)))

;; Export both code and its output by default.
(setq org-babel-default-header-args
      (cons '(:exports . "both")
            (assq-delete-all :exports org-babel-default-header-args)))

;; Enable auto-fill-mode when editing org-files.
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(provide 'org-conf)
