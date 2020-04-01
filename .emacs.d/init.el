;;; init --- Initialises emacs.

;;; Commentary:
;; The init.el for Rovanion. It is split into a couple of different files.

;;; Code:

;; Avoid collecting garbage during startup, reset later by enabling `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum)

;; Added by Package.el. Cannot be moved it seems.
(package-initialize)

;; Expand path to binaries installed in home folder.
(setenv "PATH" (concat (getenv "PATH") ":~/.local/bin"))
(setq exec-path (append exec-path '("~/.local/bin")))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Automatically download packages requiered for this conf.
(require 'packages)
;; Load text auto complete configuration.
(require 'company-conf)
;; Helm auto completion of emacs actions.
(require 'helm-conf)
;; Load language specific configurations.
(require 'python-conf)
(require 'go-conf)
(require 'clojure-conf)
(require 'nginx-conf)
(require 'markdown-conf)
(require 'web-js-conf)
(require 'typescript-conf)
(require 'elisp-conf)
(require 'reason-conf)
(require 'shell-conf)
;; Load conf for mail client.
;(require 'mu4e-conf)
;; Conf for directory listing mode.
(require 'dired-conf)
(require 'org-conf)
(require 'spelling)
;; Code which tries to make *compile* show only on errorcodes != 0
(require 'only-display-compile-on-error)
;; Tell emacs "customizations" to write to the appropriate folder.
(setq custom-file "~/.emacs.d/lisp/custom.el")
;; Import project specific settings from the .editorconfig file.
(require 'editorconfig)
(editorconfig-mode 1)

(projectile-mode +1)


;; Put scroll bar on the right in graphical mode, also remove toolbars.
(defun graphical-fixes (_)
  (when (display-graphic-p)
    (set-scroll-bar-mode 'right)))
(add-to-list 'after-make-frame-functions #'graphical-fixes)

;; We never want that menu bar visible.
(defun hide-toolbars (_)
  (menu-bar-mode -1)
  (tool-bar-mode -1))
;; Run when new windows/frames are created with emacsclient.
(add-hook 'after-make-frame-functions 'hide-toolbars)
;; And also if we start normal emacs.
(hide-toolbars nil)


;; Make scrolling by mouse linear
(setq mouse-wheel-progressive-speed nil)
;; And then scroll only one line per scroll event. Great for laptops.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))


;; Indentation galore!
(setq-default tab-width 2
              indent-tabs-mode t
              js-indent-level tab-width
              sh-basic-offset tab-width
              sh-indentation tab-width
              sgml-basic-offset tab-width
              python-indent tab-width
              web-mode-markup-indent-offset tab-width
              nginx-indent-level tab-width
              css-indent-offset tab-width)

;; Color paranthesis in all the colors of the rainbow!
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; And then highlight the parenthesis.
(show-paren-mode 1)

;; Start octave-mode when opening a file labeled .octave or .m
(add-to-list 'auto-mode-alist '("\\.octave\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; Turn on abbrevs, auto-fill and font-lock.
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))


;; Run tex-mode when a .latex-file is opened
(add-to-list 'auto-mode-alist '("\\.latex\\'" . LaTeX-mode))
;; Use visual-line-mode in latex-mode
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; Provides the minor mode which runs the compile comand on save.
(add-hook 'LaTeX-mode-hook 'recompile-on-save-mode)


;; Remove trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Surpress emacs init screen
(setq inhibit-startup-screen t)

;;;; This snippet enables lua-mode.
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Should be a fix for <dead-acute> is undefined.
(require 'iso-transl)


;; Fix bug in terminal flycheck with company-mode.
(add-hook 'flycheck-mode-hook
          (lambda ()
            (when (display-graphic-p)
              (setq-local flycheck-indication-mode nil))))
;; We're writing C++11, and we want want flycheck on by default.
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++11")
            (flycheck-mode)))


;; Don't litter the fs with temporary files but put them in a central folder.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                  (concat user-emacs-directory "auto-saves/")) t)))
(setq backup-by-copying t    ; Copy all files, don't rename them.
      delete-old-versions t  ; Don't ask to delete old versions.
      kept-new-versions 6
      kept-old-versions 2
      version-control t      ; Use versioned backups.
      vc-make-backup-files t ; Make backups of version controlled files.
      auto-save-default t    ; auto-save every buffer that visits a file
      auto-save-timeout 15   ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 150); number of keystrokes between auto-saves (default: 300)



;; Make align-regexp use spaces instead of tab characters.
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; Configure yasnippet
(yas-global-mode 1)

;; Auto wrap comments
(require 'newcomment)
(setq fill-column 66)
(setq comment-auto-fill-only-comments t)
(auto-fill-mode t)

;; Use diff-mode in commit messages.
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

;; Troll coworkers - use Emacs at work for csharp!
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(setq omnisharp-server-executable-path '~/source/OmniSharp/OmniSharp/bin/Debug/OmniSharp.exe)


;; Reload Firefox page through MozRepl.
(require 'moz)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(defun auto-reload-firefox-on-after-save-hook ()
  (add-hook 'after-save-hook
            '(lambda ()
               (interactive)
               (comint-send-string (inferior-moz-process)
                                   "setTimeout(BrowserReload, \"100\");"))
            'append 'local)) ; buffer-local

(add-hook 'toml-mode-hook 'auto-reload-firefox-on-after-save-hook)

;; Tabs for indentation, spaces for alignment.
(smart-tabs-insinuate 'c 'c++ 'java 'cperl 'nxml 'ruby)

;; We can handle it!
(put 'upcase-region 'disabled nil)

;; Disable cursor blinking, it was driving me mad!
(setq visible-cursor nil)

;; Increase the kill ring size.
(setq kill-ring-max 200)

;; Mac OSX is wierd
(setq mac-command-key-is 'super)

;; Make CamelCased subwords count as words.
(add-hook 'prog-mode-hook #'subword-mode)

;; Fonts for non-terminal emacs
(add-to-list 'default-frame-alist
             '(font . "Terminus-10"))

;; Tramp into alpine docker containers: https://github.com/emacs-pe/docker-tramp.el#troubleshooting
(require 'docker-tramp-compat)
;; Faster than the default, which is scp.
(setq tramp-default-method "ssh")

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(require 'interactive-commands)

;; Collection of keybinding customizations.
;; Should be the (almost) last thing requiered in init.el.
(require 'keybinds)

;; Make emacs only collect garbage when idling.
(require 'gcmh)
(gcmh-mode 1)

(provide 'init)
;;; init.el ends here
