
(set-background-color "black")
(setq frame-background-mode 'dark)


(require-theme 'afternoon t)

(custom-set-faces
 '(org-indent               ((t (:background "#111" :foreground "#111"))))
)


;; Keep these commented out for now.
(defun my/color-customizations ()
    (custom-set-faces
     `(default                  ((t (:background "black"))))
     `(helm-selection           ((t (:background "brightred"))))
     `(helm-ff-file             ((t (:foreground "white"))))
     `(diff-changed-unspecified ((t (:background "black"))))
     `(diff-indicator-changed   ((t (:background "black"))))
     `(diff-indicator-removed   ((t (:background "black"))))
     `(diff-indicator-added     ((t (:background "black"))))
     `(diff-hunk-header         ((t (:background "black"))))
     `(diff-refine-changed      ((t (:background "black"))))
     `(font-lock-string-face    ((t (:foreground "#bfffff"))))
     `(variable-pitch           ((t (:foreground "white"))))
     `(sh-heredoc               ((t (:background "black" :foreground "white"))))
     `(magit-blame-highlight    ((t (:background "#111"))))))

(provide 'theme)
