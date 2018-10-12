;; ocalm et. al.
(use-package utop)
(use-package tuareg
  :config
  (add-hook 'tuarag-model-hook (lambda ()
                                 (utop-minor-mode)))
                                 (add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\)?\\'" . tuareg-dune-mode))
  )

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

(provide 'ocaml-pack)

