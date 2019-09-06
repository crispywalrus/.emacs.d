
;; lesser used hence lesser customized stuff
(use-package markdown-mode
  :pin melpa-stable
  :init
  (setq
   auto-mode-alist  (cons '("\\.md$" . markdown-mode) auto-mode-alist)
   auto-mode-alist  (cons '("\\.markdown$" . markdown-mode) auto-mode-alist)))

(use-package pandoc-mode)

(provide 'markup-buffs)
