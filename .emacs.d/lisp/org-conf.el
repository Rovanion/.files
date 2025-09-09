;;;org-conf -- Personal configuration for Org-mode.

;;; Code:

;; To enable the easy templates, with it enabled "<s" followed by TAB
;; will insert a source block. This behaviour was disabled by default in 9.2.
(with-eval-after-load 'org
  (if (version< "9.2" org-version)
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
     (ditaa      . t)
     (shell      . t)
     (python     . t)
     (clojure    . t)
     (emacs-lisp . t))))

;; The below path will not work on non-Debian machines, but I don't care right now.
(setq org-ditaa-jar-path "/usr/bin/ditaa")

;; Always use python3
(setq org-babel-python-command "python3")

;; Never prompt whether or not to run code.
(setq org-confirm-babel-evaluate (lambda (lang body) nil))
;; Don't automatically indent files
(setq org-adapt-indentation nil)
;;; And don't indent source blocks.
(setq org-edit-src-content-indentation 0)

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

(defun org-repeat-previous-source-block ()
  "Returns the previous source block in its entirety."
  (interactive)
  (save-excursion
    (org-babel-previous-src-block)
    (let ((element (org-element-at-point)))
      (when (org-element-type-p element 'src-block)
        (let* ((lang           (org-element-property :language   element))
               (switches       (org-element-property :switches   element))
               (params         (org-element-property :parameters element))
               (code           (org-element-property :value      element))
               (header-content (mapconcat #'identity (delq nil (list lang switches params)) " ")))
          (format
           (concat "#+begin_src %s\n"
                   "%s"
                   "#+end_src\n")
           header-content code))))))

(defun org-previous-source-block-headers ()
  "Returns the previous source block's headers."
  (save-excursion
    (org-babel-previous-src-block)
    (let ((element (org-element-at-point)))
      (when (org-element-type-p element 'src-block)
        (let* ((lang           (org-element-property :language   element))
               (switches       (org-element-property :switches   element))
               (params         (org-element-property :parameters element)))
          (delq nil (list lang switches params)))))))

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

;; Custom PATH to GPFS binaries on NSC machines.
(connection-local-set-profile-variables 'remote-path-with-gpfs
 '((tramp-remote-path . ("/usr/lpp/mmfs/bin/" tramp-default-remote-path))))
(connection-local-set-profile-variables 'remote-path-with-root-bin
 '((tramp-remote-path . ("/root/bin/" tramp-default-remote-path))))
;; In actuality applied to all connections, see: https://emacs.stackexchange.com/questions/78846/
(connection-local-set-profiles
 '(:application tramp) 'remote-path-with-gpfs)
(connection-local-set-profiles
 '(:application tramp) 'remote-path-with-root-bin)

(setq explicit-shell-file-name "/bin/sh")

;;; Enable org's indent by level.
(setq org-indent-indentation-per-level 1)
(setq org-startup-indented t)

;;; Remove empty line between source code and results block
(defun my-remove-line (_a _b)
  (save-excursion
    (previous-line)
    (beginning-of-line)
    (when (looking-at-p "\n")
      (kill-line))))

(advice-add 'org-babel--insert-results-keyword :before #'my-remove-line)

(require 'org)
;;; Allow a possessive s after emphasis charcters, to so that ~datil~s is correctly recognised and translated to `datil`s in Markdown.
;;; https://emacs.stackexchange.com/a/13828/2325
(setcar (nthcdr 1 org-emphasis-regexp-components) "-[:space:].,:!?;'\")}\\[s")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

(provide 'org-conf)
