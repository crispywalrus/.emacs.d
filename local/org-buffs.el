;; org-buffs.el --- org additions and configuration -*- lexical-binding: t -*-

(require 'project-management)

(defun crispy:troll-org-directory (dir) (f-expand dir org-directory))

(use-package org
  :ensure t
  :init
  (setq org-log-done t
        org-directory (expand-file-name "~/Documents/org")
        org-default-notes-file (f-join org-directory "notes.org")
        org-agenda-files (list (f-join org-directory "agenda"))
        org-todo-keywords
        '((sequence "TODO(t)" "READY(r)" "INPROGRESS(p)" "NEXT(n)" "BLOCKED(b)" "|" "CANCELED(c)" "DONE(d)")
          (sequence "IDEATE" "REFINE" "DOCUMENT" "PROMOTED"))
        org-todo-keyword-faces
        '(("TODO" . (:foreground "GoldenRod" :weight bold))
          ("READY" . (:foreground "IndianRed1" :weight bold))   
          ("INPROGRESS" . (:foreground "OrangeRed" :weight bold))
          ("NEXT" . (:foreground "deep pink" :weight bold)) 
          ("BLOCKED" . (:foreground "coral" :weight bold)) 
          ("CANCELED" . (:foreground "LimeGreen" :weight bold))))
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)
         ("H-l" . org-store-link)
         ("H-c" . org-capture)
         ("H-a" . org-agenda)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ocaml . t)
   (sql . t)))

;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)

;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

;; Fix an incompatibility between the ob-async and ob-ipython packages
(setq ob-async-no-async-languages-alist '("ipython"))

(use-package kanban)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-journal
  :ensure t
  :custom
  (org-journal-dir (f-join org-directory "journal"))
  (org-journal-file-format "cv-%Y%m%d")
  (org-journal-time-format ""))

(use-package org-elisp-help)
(use-package org-dashboard)

;; thet ox mode name denotes an org exporter
(use-package ox-pandoc)
(use-package ox-reveal
  :config (setq org-reveal-root (expand-file-name (f-join org-directory "reveal.js"))))

(defvar crispy:created-property-string "
  :PROPERTIES:
  :CREATED: %U
  :END:")

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("H-n" . org-capture))
  :config
  (setq org-projectile-projects-file
        (f-join org-directory "projectile.org"))
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates)
  :ensure t)

(setq diary-file (f-join org-directory "diary"))

;; read the news in emacs
(use-package elfeed)
(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list  (f-join org-directory "elfeed.org"))))

(provide 'org-buffs)
