;;;;
;; Automatically download the required packages for emacs
;;;;

(setq package-list
      '(flycheck
        go-mode
        company-go
        cmake-mode
        yasnippet
        glsl-mode
        puppet-mode
        web-mode
        rainbow-delimiters
        nginx-mode
        lua-mode
        yaml-mode
        visual-regexp
        markdown-mode
        markdown-toc
        recompile-on-save
        toml-mode
        mu4e-maildirs-extension
        smart-tabs-mode
        cider
        smartparens
        helm
        helm-gtags
        php-mode
        flyspell-lazy
        flyspell-correct-helm
        gnuplot-mode
        browse-kill-ring
        editorconfig
        jdee
        rust-mode
        atomic-chrome
        dockerfile-mode
        tide
        groovy-mode
        eval-sexp-fu
        highlight              ; An unlisted dependency of eval-sexp-fu
        string-inflection      ; Switching BETWEEN different_variable-namingConventions.
        docker-tramp           ; Tramp into docker containers.
        reason-mode            ; An OCaml dialect for JS transpilation.
        merlin                 ; OCaml auto completion.
        dired-single           ; To reuse the same dired buffer while browsing.
        org                    ; Document format for organizing your life.
        nix-mode               ; A functionally pure package management system with a dedicated language.
        projectile             ; Operating within the context of a source code repository.
        geiser                 ; Scheme IDE and REPL.
        geiser-guile           ; Guile Scheme specifics for Geiser.
        guix                   ; Another functionally pure package management system.
        gcmh                   ; Makes emacs only collect garbage when idling.
        inf-clojure            ; Inferior Clojure mode for babashka.
        org-jira               ; Integrate org with Jira.
        ox-jira                ; Export org-files to jira syntax.
        which-key              ; Shows help text after prefix-key is pressed.
        dash                   ; Elisp utility functions/macros like -> and ->>.
        powershell             ; Editing and inferior mode.
        direnv                 ; Applying direnv directives based on file location.
        multiple-cursors       ; Allows edits to multiple points in a buffer at the same time.
        zig-mode               ; Zig, the programming language.
        elixir-mode            ; Elixir, the programming language.
        git-modes              ; Superseeds gitignore-mode. Also provides gitattributes- and gitconfig-mode.
        eglot                  ; Language server client.
        jump-char              ; Jump to next character X.
        adaptive-wrap          ; Indent wrapped lines for reduced visual clutter.
        pytest                 ; Python py.test wrapper.
        magit                  ; Git interface.
        ))

;; Repositories
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu-elpa"     . "http://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 10)
                                   ("gnu-elpa"     . 5)
                                   ("melpa"        . 0)))

;; The following line was previously on emacs 27+. Shit will probably break on some computer.
(package-initialize)

;; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'packages)
;;; packages.el ends here
