;;; Code:

(with-eval-after-load 'python-mode
  (eglot-ensure))

;; Use ipython with elpy to enable autoreloading of modules,
;; see: https://emacs.stackexchange.com/questions/13476
(if (executable-find "ipython3")
    (setq
     python-shell-interpreter "ipython3"
     python-shell-interpreter-args "--simple-prompt --pprint"))

;; To enable flashing of the sent region in the code buffer
(require 'eval-sexp-fu)
(setq elpy-shell-echo-output t)

(setq python-guess-indent t)

(provide 'python-conf)
;;; python-conf.el ends here
