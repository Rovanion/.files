;;; elisp-conf -- Personal configuration for elisp files.

;;; Code:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (setq indent-tabs-mode nil)
            ;; Pretty-print eval'd expressions.
            (define-key emacs-lisp-mode-map
              "\C-x\C-e" 'pp-eval-last-sexp)
            ;; Recompile if .elc exists.
            (add-hook (make-local-variable 'after-save-hook)
                      (lambda ()
                        (byte-recompile-directory default-directory)))
            (define-key emacs-lisp-mode-map
              "\r" 'reindent-then-newline-and-indent)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(provide 'elisp-conf)
