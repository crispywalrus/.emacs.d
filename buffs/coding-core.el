
;; stackoverflow is great but why leave emacs to search it?
(use-package sx
  :init (require 'bind-key)
  :config
  (bind-keys
   :prefix "C-c s"
   :prefix-map my-sx-map
   :prefix-docstring "Global keymap for SX."
   ("q" . sx-tab-all-questions)
   ("i" . sx-inbox)
   ("o" . sx-open-link)
   ("u" . sx-tab-unanswered-my-tags)
   ("a" . sx-ask)
   ("s" . sx-search)))

(use-package smartparens
  :diminish smartparens-mode
  :config (require 'smartparens-config))
  ;; :init
  ;; (setq sp-interactive-dwim t)
  ;; :config
  ;; (require 'smartparens-config)
  ;; (sp-use-smartparens-bindings)
  ;; (sp-pair "(" ")" :wrap "s-(") ;; how do people live without this?
  ;; (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  ;; (sp-pair "{" "}" :wrap "s-{")
  ;; (bind-key "H-<left>" nil smartparens-mode-map)
  ;; (bind-key "H-<right>" nil smartparens-mode-map)

  ;; (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  ;; (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

(use-package company
  :diminish company-mode)

(use-package lsp-mode
  :init (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-scala
  :after scala-mode
  :demand t
  :hook (scala-mode . lsp))

(use-package popup)

(use-package company-lsp)


;; (defun company-backends-for-buffer ()
;;   "Calculate appropriate `company-backends' for the buffer.
;; For small projects, use TAGS for completions, otherwise use a
;; very minimal set."
;;   (projectile-visit-project-tags-table)
;;   (cl-flet ((size () (buffer-size (get-file-buffer tags-file-name))))
;;     (let ((base '(company-keywords company-dabbrev-code company-yasnippet)))
;;       (if (and tags-file-name (<= 20000000 (size)))
;;           (list (push 'company-etags base))
;;         (list base)))))

(provide 'coding-core)
