;;;org-conf -- Personal configuration for Org-mode.

;;; Code:

;; To enable the easy templates, with it enabled "<s" followed by TAB
;; will insert a source block. This behaviour was disabled by default in 9.2.
(with-eval-after-load 'org
  (if (version< "9.1" org-version)
      (require 'org-tempo)))

;; Enable Clojure as a language in Org source blocks.
(with-eval-after-load 'org
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider))

;; Active Babel languages.
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R          . t)
     (js         . t)
     (ruby       . t)
     (shell      . t)
     (python     . t)
     (clojure    . t)
     (emacs-lisp . t))))

;; Never prompt whether or not to run code.
(setq org-confirm-babel-evaluate (lambda (lang body) nil))
;; Don't automatically indent files
(setq org-adapt-indentation nil)

;; Avoid accidentally editing folded regions, say by adding text after an Org “⋯”.
(setq org-catch-invisible-edits 'show)

(with-eval-after-load 'org
  ;; Cache results by default.
  (setq org-babel-default-header-args
        (cons '(:cache . "yes")
              (assq-delete-all :cache org-babel-default-header-args)))
  ;; Export both code and its output by default.
  (setq org-babel-default-header-args
        (cons '(:exports . "both")
              (assq-delete-all :exports org-babel-default-header-args))))

;; Enable auto-fill-mode when editing org-files.
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(defun org-previous-source-block ()
  "Returns the previous source block in its entirety."
  (save-excursion
    (org-babel-previous-src-block)
    (let ((element (org-element-at-point)))
      (when (eq (car element) 'src-block)
        (let* ((content  (cadr element))
               (lang     (plist-get content :language))
               (switches (plist-get content :switches))
               (parms    (plist-get content :parameters))
               (value    (plist-get content :value)))
          (delq nil (list lang switches parms "\n" )))
        ))))

(defun org-previous-source-block-headers ()
  "Returns the previous source block's headers."
  (save-excursion
    (org-babel-previous-src-block)
    (let ((element (org-element-at-point)))
      (when (eq (car element) 'src-block)
        (let* ((content  (cadr element))
               (lang     (plist-get content :language))
               (switches (plist-get content :switches))
               (parms    (plist-get content :parameters)))
          (delq nil (list lang switches parms)))))))

(defun org-previous-source-block-headers-string ()
  "Returns the previous source block's headers as a string."
  (mapconcat #'identity
             (org-previous-source-block-headers)
              " "))

(defun org-previous-source-block-without-content ()
  "Returns the previous source block, except its contents."
  (format
   (concat "#+begin_src %s\n"
           "\n"
           "#+end_src\n")
   (org-previous-source-block-headers-string)))

(defun org-repeat-previous-source-block-without-content ()
  "Inserts the previous source block, except the actual code in
the block, into the buffer."
  (interactive)
    (when-let (result (org-previous-source-block-without-content))
      (insert result)
      (previous-line 2)))

(defun toggle-org-pdf-export-on-save ()
  (interactive)
  (if (memq ''org-latex-export-to-pdf after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-latex-export-to-pdf t)
        (message "Disabled org pdf export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-latex-export-to-pdf nil t)
    (message "Enabled org pdf export on save for current buffer...")))

;; Do wrap lines.
(setq org-startup-truncated nil)

;; Export only the current subtree by default rather than the whole buffer.
(setq org-export-initial-scope 'subtree)

(with-eval-after-load 'org
  (require 'ox-md)     ; Export to markdown
  (require 'ox-jira))  ; and Jira.


(provide 'org-conf)
