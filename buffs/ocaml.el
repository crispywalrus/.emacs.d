;; ocaml.el  -*- lexical-binding: t -*-
;; emacs configuration and editing mode for ocalm programming. 

(require 'coding-support)
(require 'dune)
(require 'dune-flymake)

;; it's annoying that it's not possible to preload any of the dune support and get auto mode selection for dune-project files.
(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)dune-project\\(?:\\.inc\\)?\\'" . dune-mode))

(provide 'ocaml)

