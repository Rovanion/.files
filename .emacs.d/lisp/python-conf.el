;;; Code:

;; Use Python 3 by default
;; (setq python-remove-cwd-from-path nil)
;; (custom-set-variables
;;  '(python-python-command "python3")
;;  '(python-shell-interpreter "python3"))
;; (defun python-shell-parse-command ()
;;   "Return the string used to execute the inferior Python process."
;;   "/usr/bin/python3 -i")

(setq jedi:environment-root "jedi-virtualenv")
;; (setq jedi:environment-virtualenv
;;       (list "virtualenv" "--system-site-packages" "ENV" "-p" "/usr/bin/python3"))

;; Set up the python interpretern needed for company.
;; (defun run-python-once ()
;;   (remove-hook 'python-mode-hook 'run-python-once)
;;   (run-python))
;; (add-hook 'python-mode-hook 'run-python-once)

(require 'elpy)
(elpy-enable)

;; Use ipython with elpy to enable autoreloading of modules,
;; see: https://emacs.stackexchange.com/questions/13476
(if (executable-find "ipython3")
    (setq
     python-shell-interpreter "ipython3"
     python-shell-interpreter-args "--simple-prompt --pprint"))

;; To enable flashing of the sent region in the code buffer
(require 'eval-sexp-fu)
(setq elpy-shell-echo-output t)

;; Run Jedi and Elpy when we edit python files.
;; (add-hook 'python-mode-hook #'jedi-mode)
(add-hook 'python-mode-hook #'elpy-mode)

(setq python-guess-indent t)


(provide 'python-conf)
;;; python-conf.el ends here
