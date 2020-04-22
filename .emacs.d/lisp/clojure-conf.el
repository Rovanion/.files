;;;clojure-conf -- Personal configuration for Clojure and Clojurescript.

;;; Code:

;; Vertical align of arguments in sexps
(setq clojure-align-forms-automatically t)

(setq cider-cljs-lein-repl
			"(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;; Use company-mode with cider.
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(setq cider-completion-annotations-include-ns t)

;; Use company in clojure-mode
(add-hook 'clojure-mode-hook #'company-mode)

;; Cider diet was some sort of experiment I used in 2017, remove it if you haven't used it by 2021.
(setq cider-diet-path
			(expand-file-name (concat (getenv "HOME") "/.local/bin/cider-diet.jar")))

(defun cider-diet-jack-in ()
	(interactive)
	(let* ((cider-diet-process (start-process "cider-diet-nrepl" "*cider-diet-nrepl*"
																						"java" "-jar" cider-diet-path "7888")))
		(accept-process-output cider-diet-process)
		    (cider-connect "localhost" 7888)))

;; Don't open up a prompt each time M-å is used.
(setq cider-prompt-for-symbol nil)

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (inhibit-field-text-motion t)
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (and (not (bobp))
                            (= end (save-excursion
                                     (comment-forward (point-max))
                                     (point))))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (ignore-errors
                  (while (looking-at-p comment-start-skip)
                    (forward-char -1)))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (eq 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (save-excursion
       (comment-region l r))
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))


;;;; Babashka is clojure for scripting, Graal compiled Clojure with
;;;; help. As of 2020-04-03 it has no NREPL implementation and thus
;;;; not CIDER, but we can use inf-clojure for it.
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . inf-clojure-minor-mode))

;; Show function signatures down in the echo area.
(add-hook 'inf-clojure-mode-hook #'eldoc-mode)

(provide 'clojure-conf)
;;; clojure-conf.el ends here
