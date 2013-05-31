;; Put scroll bar on the right in graphical mode.
(set-scroll-bar-mode 'right)
;; Hide the menu- and tool-bar in graphical mode.
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Make scrolling by mouse linear
(setq mouse-wheel-progressive-speed nil)
;; And then scroll only one line per scroll event. 
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Each level of indention in JS is 2 spaces.
(setq js-indent-level 2)

;; Color paranthesis in all the colors of the rainbow!
;; Requires the fallowing plugin http://www.emacswiki.org/emacs/RainbowDelimiters
(add-to-list 'load-path "~/.emacs.d/")
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode 1)

;; And then highlight the parenthesis
(show-paren-mode 1)

;; Auto complete mode. Fetched from the fallowing URL:
;; http://cx4a.org/software/auto-complete/
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
;; Load the default config
(ac-config-default)
;; Say with a stern voice that capital letters matter.
;;(setq ac-ignore-case nil)
;; Start showing completions with no delay.
(setq ac-delay 0)
(setq ac-auto-start 1)
;; Try to use spell correction if there are no matches.
;; TODO: This does not work, fix it.
(setq ac-use-fuzzy 1)


;; Start octave-mode when opening a file labeled .octave or .m
(autoload 'octave-mode "octave-mode" "Loding octave-mode" t)
(add-to-list 'auto-mode-alist '("\\.octave\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; Turn on abbrevs, auto-fill and font-lock.
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))
;; Turn on autocomplete-mode when octave-mode is running
(add-to-list 'ac-modes 'octave-mode)

;; Run tex-mode when a .latex-file is opened
(add-to-list 'auto-mode-alist '("\\.latex\\'" . tex-mode))

;; Surpress emacs init screen
(setq inhibit-startup-screen t)


;; Defines a mode which automatically runs recompile after save
(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer) 
      (recompile))))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if compile-on-save-mode
    (progn  (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook 'compile-on-save-start nil t))
      (kill-local-variable 'after-save-hook)))


