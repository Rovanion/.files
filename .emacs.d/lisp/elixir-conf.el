(with-eval-after-load 'elixir-mode
  (eglot-ensure))

(add-to-list 'eglot-server-programs '(elixir-mode "~/.local/lib/elixir-ls/language_server.sh"))

(provide 'elixir-conf)
