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


(add-hook 'git-commit-setup-hook
          (lambda ()
            (setq fill-column 72)))

;; Don't show second window with diff. I already add my own with the verbose flag to git commit.
(setq magit-commit-show-diff nil)

(provide 'git-conf)
