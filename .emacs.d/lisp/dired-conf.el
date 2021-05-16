;;;;
;; Settings for dired-mode.
;;;;

;; Reuse buffer when browsing. Requires manual rebinding of keys
;; though which is done in lisp/keybinds.el.
(require 'dired-single)

;; Use trashcan instead of deleting directly.
(setq delete-by-moving-to-trash t)

(defun dired-open-xdg ()
  "Try to run `xdg-open' to open the file under point."
  (interactive)
  (if (executable-find "xdg-open")
      (let ((file (ignore-errors (dired-get-file-for-visit))))
        (start-process "dired-open" nil
                       "xdg-open" (file-truename file)))
    nil))


(provide 'dired-conf)
;;; dired-conf.el ends here
