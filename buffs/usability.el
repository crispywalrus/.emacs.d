;; usability.el -*- lexical-binding: t -*-

;; stackoverflow is an integral part of the coding process. so why
;; leave emacs to search it?
(use-package sx
  :bind (("H-x q" . sx-tab-all-questions)
         ("H-x i" . SX-inbox)
         ("H-x o" . sx-open-link)
         ("H-x u" . sx-tab-unanswered-my-tags)
         ("H-x a" . sx-ask)
         ("H-x s" . sx-search)))

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

;; change word bounderies to include lower case to upper case
;; transitions inside camel cased words.
(use-package subword
  :init (global-subword-mode t))

(use-package popwin
  :config (popwin-mode 1))

(use-package memoize)

(use-package counsel)

(provide 'usability)
