;; org-buffs.el --- org additions and configuration -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This is an attempt at a usable org mode configuration.  Currently
;;; this means some keyword config and few key bindings.  Then
;;; additions to org-babel to use scala and sql.  Org-roam v2 has
;;; replaced org-journal and a lot of my manual org file
;;; editing.  Finally org-projectile rounds out the practical code.

;;; Code:

(require 'f)
(require 'coding-support)

(defgroup org-buffs nil
  "Setup and configure `org-mode' and all loaded extensions."
  :group 'buffs
  :prefix "org-buffs:")

(defcustom org-buffs:orgfiles-tree (expand-file-name "~/.org")
  "The root of the `org-mode' file tree."
  :type 'file
  :group 'org-buffs)

(defcustom org-buffs:agenda-dirs (list (f-join org-buffs:orgfiles-tree "agenda") (f-join devel:dev-tree-root "agenda"))
  "A list of directories in which to file our `org-mode' agenda files."
  :type '(list file)
  :group 'org-buffs)

(defcustom org-buffs:roam-dir (f-join org-buffs:orgfiles-tree "roam")
  "The root of the `roam' database directory."
  :type 'file
  :group 'org-buffs)

(use-package org
  :ensure t
  :init
  (setq org-log-done t
        org-directory org-buffs:orgfiles-tree
        org-default-notes-file (f-join org-directory "notes.org")
        org-agenda-files org-buffs:agenda-dirs
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
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

(defun org-bufs:dangle-org-directory (dir)
  "Expand and create, if needed, DIR in the org-directory tree."
  (let (dangled-dir (f-expand dir org-directory))
    (f-touch dangled-dir)
    dangled-dir))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scala . t)
   (ocaml . t)
   (sql . t)))

(use-package ob-async
  ;; Fix an incompatibility between the ob-async and ob-ipython packages
  :init (setq ob-async-no-async-languages-alist '("ipython")))

(use-package org-superstar
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-kanban)

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

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-priority-highest 0
        org-priority-default 2
        org-priority-lowest 4)
  (setq org-fancy-priorities-list '(
                                    (?0 . "P0")
                                    (?1 . "P1")
                                    (?2 . "P2")
                                    (?3 . "P3")
                                    (?4 . "P4"))))

(provide 'org-buffs)
;;; org-buffs.el ends here.
