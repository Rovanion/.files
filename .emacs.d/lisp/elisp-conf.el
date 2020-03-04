;;; elisp-conf -- Personal configuration for elisp files.

;;; Code:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (setq indent-tabs-mode nil)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(defun elisp-showdoc (f)
  (interactive (list (thing-at-point 'symbol t)))
  (message
   "%s"
   (let* ((doc-list      (split-string (documentation (intern f)) "\n"))
          (number-lines  (min (- (floor (* max-mini-window-height (frame-height))) 2)
                              (- (length doc-list) 2)))
          (subset        (concatenate 'list
                                      (last doc-list)
                                      '("")
                                      (subseq doc-list 0 number-lines)))
          (pruned-subset (if (string-equal (car (last subset)) "")
                             (butlast subset)
                             subset)))
     (mapconcat #'identity pruned-subset "\n"))))

(define-advice elisp-get-fnsym-args-string (:around (orig-fun sym &rest r) docstring)
  "If SYM is a function, append its docstring."
  (concat
   (apply orig-fun sym r)
   (let* ((doc     (and (fboundp sym) (documentation sym 'raw)))
          (oneline (and doc (substring doc 0 (string-match "\n" doc)))))
     (and oneline
          (stringp oneline)
          (not (string= "" oneline))
          (concat "  |  " (propertize oneline 'face 'italic))))))

(provide 'elisp-conf)
