;; 
;; manipulating markdown (mostly) or any markup format that's not org
(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))
    
(use-package pandoc-mode
  :hook markdown-mode)

(use-package yaml-mode
  :config (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

(provide 'markup)
