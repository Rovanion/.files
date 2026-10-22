(when (package-installed-p 'magit)
  (require 'magit))

(custom-set-faces
 ;; Magit
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-section-highlight ((((type tty)) nil)))
 ;; Diff-mode
 '(diff-added          ((((type tty)) (:foreground "green" :background "default"))))
 '(diff-refine-added   ((((type tty)) (:foreground "green" :background "black"))))
 '(diff-removed        ((((type tty)) (:foreground "red"   :background "default"))))
 '(diff-refine-removed ((((type tty)) (:foreground "red"   :background "black"))))
 '(diff-changed        ((((type tty)) (:foreground "white" :background "default"))))
 '(diff-header         ((((type tty)) (:foreground "grey"  :background "default"))))
 '(diff-file-header    ((((type tty)) (:foreground "white" :background "default")))))


;; Use diff-mode in commit messages for diff color, log-edit-mode for message formatting guidelines.
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . log-edit-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))


(defun git-commit-set-fill-column ()
    (setq fill-column 72))

;; Don't show second window with diff. I already add my own with the verbose flag to git commit.
(setq magit-commit-show-diff nil)

(setq git-commit-setup-hook
      (list #'git-commit-ensure-comment-gap
            #'git-commit-save-message
            #'git-commit-setup-capf
            #'git-commit-setup-changelog-support
            #'git-commit-setup-auto-fill
            #'git-commit-propertize-diff
            #'bug-reference-mode
            #'git-commit-set-fill-column))

;; Use the only viable option for terminal emacs by default.
(setq magit-blame-echo-style 'headings)

(provide 'git-conf)
