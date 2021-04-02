;; ocaml-reasonml.el  -*- lexical-binding: t -*-

;; emacs configuration and editing mode for ocalm and reasonml programming. 

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))


(require 'coding-support)
(require 'dune)
(require 'dune-flymake)
(require 'lsp-ocaml)

(use-package reason-mode)

(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))

;; it's annoying that it's not possible to preload any of the dune support and get auto mode selection for dune-project files.
(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)dune-project\\(?:\\.inc\\)?\\'" . dune-mode))

(provide 'ocaml-reasonml)

