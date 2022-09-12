;;; usability.el --- improve emacs usablity  -*- lexical-binding: t -*-

;;; Commentary:
;; stackoverflow is an integral part of the coding process.  so why
;; leave Emacs to search it?

;;; Code:
(use-package sx
  :bind (("H-x q" . sx-tab-all-questions)
         ("H-x i" . SX-inbox)
         ("H-x o" . sx-open-link)
         ("H-x u" . sx-tab-unanswered-my-tags)
         ("H-x a" . sx-ask)
         ("H-x s" . sx-search)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(use-package spaceline
  :after all-the-icons
  :init (require 'spaceline-config)
        (spaceline-info-mode))

(use-package spaceline-all-the-icons
  :after spaceline
  :config (spaceline-all-the-icons-theme))

(use-package sicp)

(use-package expand-region
  :after all-the-icons
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
;;; usability.el ends here
