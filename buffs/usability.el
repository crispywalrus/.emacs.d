
;; loads key-chord, it adds a :chord symbol for use-package.
(use-package use-package-chords
  :config (key-chord-mode 1))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

;; change word bounderies to include lower case to upper case
;; transitions inside camel cased words.
(use-package subword
  :ensure nil
  :diminish subword-mode
  :init (global-subword-mode t))

(use-package popwin)

(provide 'usability)
