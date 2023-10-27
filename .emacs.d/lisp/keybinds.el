;;;;
;; Custom keybindings.
;;;;
(require 'utilities)

;; Rotate different naming conventions,
(global-unset-key (kbd "C-t"))
(global-set-key (kbd "C-t C-t") 'string-inflection-all-cycle)

(global-set-key (kbd "C-t C-s") 'windows-to-posix-path)

(define-key prog-mode-map (kbd "RET") 'newline-and-indent)
(define-key prog-mode-map (kbd "C-a") 'smart-line-beginning)
(define-key prog-mode-map (kbd "C-o") 'split-line)
(define-key prog-mode-map (kbd "C-M-o") 'open-line)

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; C, C++, Objective-C, Java
(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "RET") 'newline-and-indent)
  (define-key c-mode-map (kbd "C-a") 'smart-line-beginning)
  (define-key c-mode-map (kbd "C-o") 'split-line)
  (define-key c-mode-map (kbd "C-M-o") 'open-line))


(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "M-%")   'vr/query-replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C--") 'undo)

;; The following binds browse-kill-ring to M-y.
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Bind keys for moving to the next char of some type.
(global-set-key (kbd "M-n") 'jump-char-forward)
(global-set-key (kbd "M-p") 'jump-char-backward)

;; Remove word backwards with C-BSPC
(global-set-key "\C-h" 'backward-kill-word)

;; Company
(with-eval-after-load 'company
  (define-key company-mode-map (kbd "<backtab>") 'company-complete))

;;;
;; Bindings for going to the definition of symbols and back for a bunch of modes
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-å") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-ä") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;;; Clojure
(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "M-;") 'comment-or-uncomment-sexp-or-region))

;; CIDER and inf-clojure are two alternative IDE-like environments
(with-eval-after-load 'cider-mode
  (define-key cider-mode-map (kbd "M-å")     'cider-find-dwim)
  (define-key cider-mode-map (kbd "M-ä")     'cider-pop-back)
  (define-key cider-mode-map (kbd "C-c C-r") 'cider-eval-region)
  (define-key cider-mode-map (kbd "C-c C-b") 'cider-eval-buffer))

;; inf-clojure is the inferior of the both.
(with-eval-after-load 'inf-clojure
  (define-key inf-clojure-minor-mode-map (kbd "M-å") 'inf-clojure-symbol-at-point)
  (define-key inf-clojure-minor-mode-map (kbd "C-c C-e") 'inf-clojure-eval-last-sexp))


;; TypeScript
(with-eval-after-load 'tide
  (define-key tide-mode-map (kbd "M-å") 'tide-jump-to-definition)
  (define-key tide-mode-map (kbd "M-ä") 'tide-jump-back)
  (define-key tide-mode-map (kbd "M-ö") 'tide-fix))

;; Emacs lisp, elisp
(define-key emacs-lisp-mode-map (kbd "M-å")     'xref-find-definitions)
(define-key emacs-lisp-mode-map (kbd "M-ä")     'xref-pop-marker-stack)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'elisp-showdoc)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "M-;")     'comment-or-uncomment-sexp-or-region)

;; Shell mode
(with-eval-after-load 'sh-script  ; https://repo.or.cz/w/emacs.git/blob/HEAD:/lisp/progmodes/sh-script.el
  (define-key sh-mode-map (kbd "C-c C-b") 'sh-send-buffer)
  (define-key sh-mode-map (kbd "C-c C-e") 'sh-send-line-or-region)
  (define-key sh-mode-map (kbd "C-c C-r") 'sh-send-line-or-region))

(require 'multiple-cursors)
(global-set-key (kbd "C-t l")   'mc/edit-lines)
(global-set-key (kbd "C-t C-l") 'mc/edit-lines)
(global-set-key (kbd "C-t C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-t C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-t C-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-t a")   'mc/mark-all-like-this)

(with-eval-after-load 'flyspell
  (require 'flyspell-correct-helm)
  (define-key flyspell-mode-map (kbd "M-ö") 'flyspell-correct-previous-word-generic))

;; Pair oriented navigation
(require 'smartparens-config)
(smartparens-global-mode)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "M-)")   'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-(")   'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-}")   'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-{")   'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-j")   'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-k")   'kill-sexp)

;; Bind completion to tab in helm. Because as much as its author wants
;; to retrain my brain, my brain has not been retrained after five
;; years of use.
(define-key helm-map (kbd "TAB")   #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")   #'helm-select-action)

;; Dired
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "p")   #'dired-single-up-directory)
  (define-key dired-mode-map (kbd "^")   #'dired-single-up-directory)
  (define-key dired-mode-map (kbd "RET") #'dired-single-buffer)
  (define-key dired-mode-map [mouse-1]   #'dired-single-buffer-mouse))


;; Puppet
(with-eval-after-load 'puppet-mode
  (define-key puppet-mode-map (kbd "<backtab>") #'puppet-align-block)
  (define-key puppet-mode-map (kbd "TAB") #'puppet-indent-line))

;; Scheme
(with-eval-after-load 'geiser-mode
  (define-key geiser-mode-map (kbd "C-c C-e") #'geiser-eval-last-sexp)
  (define-key geiser-mode-map (kbd "C-c C-r") #'geiser-eval-region)
  (define-key geiser-mode-map (kbd "C-c C-b") #'geiser-eval-buffer)
  ;; Remember that you have to compile the buffer with C-c C-k before the below start working.
  (define-key geiser-mode-map (kbd "M-å")     #'geiser-edit-symbol-at-point)
  (define-key geiser-mode-map (kbd "M-ä")     #'geiser-pop-symbol-stack))

;; SQL
(with-eval-after-load 'sql-mode
  (define-key sql-mode-map (kbd "C-c C-e") #'sql-send-paragraph)
  (define-key sql-mode-map (kbd "TAB") (lambda () (interactive)(insert "\t"))))

;; Eglot/LSP, used for Python now
(with-eval-after-load 'eglot
  (define-key eglot-mode-map  (kbd "M-å")     #'xref-find-definitions)
  (define-key python-mode-map (kbd "M-ä")     #'xref-pop-marker-stack))

;; Python
(with-eval-after-load 'python-mode
  (define-key python-mode-map (kbd "C-c C-t") (lambda () (interactive)(save-buffer)(pytest-one))))

(provide 'keybinds)
;;; keybinds.el ends here
