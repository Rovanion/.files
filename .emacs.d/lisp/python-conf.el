;;; Code:

;;; Copied from python-mode.el at https://gitlab.com/python-mode-devs/python-mode
(defvar py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]*\\([biptj]+ython[^ \t\n]*\\)"
  "Detecting the shell in head of file.")

(defun py-choose-shell-by-shebang (&optional shebang)
  "Choose shell by looking at #! on the first line.

If SHEBANG is non-nil, returns the shebang as string,
otherwise the Python resp. Jython shell command name."
  (interactive)
  ;; look for an interpreter specified in the first line
  (let* (erg res)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at py-shebang-regexp)
        (if shebang
            (setq erg (match-string-no-properties 0))
          (setq erg (split-string (match-string-no-properties 0) "[#! \t]"))
          (dolist (ele erg)
            (when (string-match "[bijp]+ython" ele)
              (setq res ele))))))
    (when (called-interactively-p 'any) (message "%s" res))
    res))

;;; Homegrown code below.

(defun set-ipython-as-interpreter ()
  (when-let (ipython-bin (executable-find "ipython3" 'remote))
      (setq-local
       python-shell-interpreter ipython-bin
       python-shell-interpreter-args "--simple-prompt --pprint")))

(defun set-python-interpreter ()
  "Try to detect python interpreter by shebang. If detected, use
it if python2, use ipython3 if python3. Else try ipython3 or give
up."
  (if-let (detected-py (py-choose-shell-by-shebang))
      (if (string-match-p ".*python2" detected-py)
          (setq-local python-shell-interpreter detected-py)
        (set-ipython-as-interpreter))
    (set-ipython-as-interpreter)))

(add-hook 'python-mode-hook 'set-python-interpreter)

(with-eval-after-load 'python-mode
  (eglot-ensure))

(setq python-guess-indent t)

(provide 'python-conf)
;;; python-conf.el ends here
