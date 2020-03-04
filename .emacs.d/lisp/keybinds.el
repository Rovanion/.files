;;;;
;; Custom keybindings.
;;;;

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

;; Rotate different naming conventions,
(global-unset-key (kbd "C-t"))
(global-set-key (kbd "C-t") 'string-inflection-all-cycle)

(define-key prog-mode-map (kbd "RET") 'newline-and-indent)
(define-key prog-mode-map (kbd "C-j") 'newline)
(define-key prog-mode-map (kbd "C-a") 'smart-line-beginning)
(define-key prog-mode-map (kbd "C-o") 'split-line)
(define-key prog-mode-map (kbd "C-M-o") 'open-line)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'cc-mode)
(define-key c-mode-map (kbd "RET") 'newline-and-indent)
(define-key c-mode-map (kbd "C-j") 'newline)
(define-key c-mode-map (kbd "C-a") 'smart-line-beginning)
(define-key c-mode-map (kbd "C-o") 'split-line)
(define-key c-mode-map (kbd "C-M-o") 'open-line)


(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "M-%") 'vr/query-replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; If you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C--") 'undo)

;; The following binds browse-kill-ring to M-y.
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Bind keys for moving to the next char of some type.
(global-set-key (kbd "M-n") 'iy-go-up-to-char)
(global-set-key (kbd "M-p") 'iy-go-to-char-backward)
(setq iy-go-to-char-key-forward '¨)
(setq iy-go-to-char-key-backward '^)

;; Set up ace-window
(setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
(setq ace-jump-mode-move-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
(global-set-key (kbd "C-c C-SPC") 'ace-window)
(global-set-key (kbd "M-SPC") 'ace-jump-mode)

;; Remove word backwards with C-BSPC
(global-set-key "\C-h" 'backward-kill-word)

;; Company
(require 'company)
(define-key company-mode-map (kbd "<backtab>") 'company-complete)

;;;
;; Bindings for going to the definition of symbols and back for a bunch of modes
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-å") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-ä") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Clojure
(require 'cider-mode)
(define-key cider-mode-map   (kbd "M-å")     'cider-find-dwim)
(define-key cider-mode-map   (kbd "M-ä")     'cider-pop-back)
(define-key cider-mode-map   (kbd "C-c C-r") 'cider-eval-region)
(define-key clojure-mode-map (kbd "M-;")     'comment-or-uncomment-sexp)


;; TypeScript
(require 'tide)
(define-key tide-mode-map (kbd "M-å") 'tide-jump-to-definition)
(define-key tide-mode-map (kbd "M-ä") 'tide-jump-back)
(define-key tide-mode-map (kbd "M-ö") 'tide-fix)

;; Python
(require 'elpy)
(define-key elpy-mode-map (kbd "M-å") 'elpy-goto-definition)
(define-key elpy-mode-map (kbd "M-ä") 'pop-tag-mark)
(define-key elpy-mode-map (kbd "M-ö") 'elpy-rpc-get-names)
(define-key elpy-mode-map (kbd "C-c C-c") 'elpy-shell-send-group)
(define-key elpy-mode-map (kbd "C-c C-r") 'elpy-shell-send-region-or-buffer)

;; Emacs lisp, elisp
(define-key emacs-lisp-mode-map (kbd "M-å")     'xref-find-definitions)
(define-key emacs-lisp-mode-map (kbd "M-ä")     'xref-pop-marker-stack)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'elisp-showdoc)
(define-key emacs-lisp-mode-map (kbd "M-;") #'comment-or-uncomment-sexp)

;; Shell mode
(require 'sh-script) ; https://repo.or.cz/w/emacs.git/blob/HEAD:/lisp/progmodes/sh-script.el
(define-key sh-mode-map (kbd "C-c C-e") 'sh-send-line-or-region)

(require 'multiple-cursors)
(global-set-key (kbd "C-c c")   'mc/edit-lines)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c a")   'mc/mark-all-like-this)

(require 'flyspell-correct-helm)
(define-key flyspell-mode-map (kbd "M-ö") 'flyspell-correct-previous-word-generic)

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

;; Bind completion to tab in helm. Because as much as its author wants
;; to retrain my brain, my brain has not been retrained after five
;; years of use.
(define-key helm-map (kbd "TAB")   #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")   #'helm-select-action)

;; Dired
(require 'dired)
(define-key dired-mode-map (kbd "p")   #'dired-single-up-directory)
(define-key dired-mode-map (kbd "^")   #'dired-single-up-directory)
(define-key dired-mode-map (kbd "RET") #'dired-single-buffer)
(define-key dired-mode-map [mouse-1]   #'dired-single-buffer-mouse)


;; Puppet
(require 'puppet-mode)
(define-key puppet-mode-map (kbd "<backtab>") #'puppet-align-block)
(define-key puppet-mode-map (kbd "TAB") #'puppet-indent-line)


(provide 'keybinds)
;;; keybinds.el ends here
