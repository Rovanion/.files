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

;;; Python mode hardcodes tab-width to 8 spaces, for some ungodly reason.
;;; Try to detect indentation depth instead.

(defun how-many-region (begin end regexp &optional interactive)
    "Print number of non-trivial matches for REGEXP in region.
   Non-interactive arguments are Begin End Regexp"
    (interactive "r\nsHow many matches for (regexp): \np")
    (let ((count 0) opoint)
      (save-excursion
        (setq end (or end (point-max)))
        (goto-char (or begin (point)))
        (while (and (< (setq opoint (point)) end)
                    (re-search-forward regexp end t))
          (if (= opoint (point))
              (forward-char 1)
            (setq count (1+ count))))
        (if interactive (message "%d occurrences" count))
        count)))

(defun infer-indentation-style ()
  ;; If our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode.
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
        (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun set-tab-width ()
  (setq-local indent-tabs-mode nil)
  (infer-indentation-style))

(add-hook 'python-mode-hook #'set-tab-width)

(provide 'python-conf)
;;; python-conf.el ends here
