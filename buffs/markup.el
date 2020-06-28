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
  :hook
  markdown-mode)

(provide 'markup)
