;; org-buffs.el --- org additions and configuration -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This is an attempt at a usable org mode configuration.  Currently
;;; this means some keyword config and few key bindings.  Then
;;; additions to org-babel to use scala and sql.  Org-roam v2 has
;;; replaced org-journal and a lot of my manual org file
;;; editing.  Finally org-projectile rounds out the practical code.

;;; Code:
(use-package org
  :ensure t
  :init
  (setq org-log-done t
        org-directory (expand-file-name "~/.org")
        org-default-notes-file (f-join org-directory "notes.org")
        org-agenda-files (list (f-join org-directory "agenda") (expand-file-name "~/Devel/notes"))
        org-todo-keywords
        '((sequence "TODO(t)" "READY(r)" "INPROGRESS(p)" "NEXT(n)" "BLOCKED(b)" "|" "CANCELED(c)" "DONE(d)"))
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

(defun crispy:dangle-org-directory (dir)
    "Expand out dir in the org-directory tree."
    (f-expand dir org-directory))

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

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :config
  (add-hook 'after-init-hook 'org-roam-mode)
  (setq
   org-roam-directory (crispy:dangle-org-directory "roam")
   org-roam-dailies-directory (crispy:dangle-org-directory "daily")
   org-roam-dailies-capture-templates
     '(("d" "default" entry
        "* %?"
        :if-new (file+head "%<%Y-%m-%d>.org"
                           "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-setup))

(use-package org-elisp-help)

(use-package org-projectile
  :config
  (setq org-projectile-projects-file (f-join org-directory "agenda" "projectile.org"))
  (push (org-projectile-project-todo-entry) org-capture-templates)
  :bind (("C-c n p" . 'org-projectile-project-todo-completing-read)
         ("H-n" . 'org-projectile-project-todo-completing-read)))

(setq diary-file (f-join org-directory "diary"))

(use-package org-chef)

;; this is just stupid brilliant 
(use-package plain-org-wiki)
(provide 'org-buffs)
