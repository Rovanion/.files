;;;;
;; Settings for dired-mode.
;;;;


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

;; Reuse buffer when browsing. Replaces dired-single in Emacs 28.
(setf dired-kill-when-opening-new-dired-buffer t)

(provide 'dired-conf)
;;; dired-conf.el ends here
