;;;;
;; Settings for dired-mode.
;;;;

;; Reuse buffer when browsing. Requires manual rebinding of keys
;; though which is done in lisp/keybinds.el.
(require 'dired-single)

;; Use trashcan instead of deleting directly.
(setq delete-by-moving-to-trash t)

(provide 'dired-conf)
;;; dired-conf.el ends here
