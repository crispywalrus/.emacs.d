
;; stackoverflow is an integral part of the coding process. so why
;; leave emacs to search it?
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

(use-package popup)

(use-package lsp-mode
  :init (setq lsp-prefer-flymake nil
              lsp-enable-snippet nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(provide 'coding-core)
