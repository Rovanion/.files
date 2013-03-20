;; Each level of indention in JS is 2 spaces.
(setq js-indent-level 2)

;; Color paranthesis in all the colors of the rainbow!
;; Requires the fallowing plugin http://www.emacswiki.org/emacs/RainbowDelimiters
(add-to-list 'load-path "~/.emacs.d/")
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; And then highlight the parenthesis
(show-paren-mode)